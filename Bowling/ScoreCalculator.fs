namespace Bowling

[<Measure>]
type point

[<Measure>]
type frame

[<Measure>]
type fallenPins

type ScoreSheet = Frame list

and Frame =
    | Strike
    | Spare of int<fallenPins>
    | Open of int<fallenPins> * int<fallenPins>
    | Extra of int<fallenPins>

[<RequireQualifiedAccess>]
type RollResult =
    | Strike
    | Open
    | Spare

[<RequireQualifiedAccess>]
type GameState =
    | NotStartedYet
    | OpenFrameStarted of Count: int<fallenPins> * Frame: int<frame>
    | FrameFinished of Frame: int<frame>
    | NeedsExtraRolls of Count: int
    | GameFinished

[<RequireQualifiedAccess>]
type ScoreState =
    | None
    | Next of Bonus: RollResult list
    | NextAndFollowing of Bonus1: RollResult list * Bonus2: RollResult list

type Game = {
    Score: int<point>
    ScoreState: ScoreState
    GameState: GameState
} with

    static member init() = {
        Score = 0<point>
        ScoreState = ScoreState.None
        GameState = GameState.NotStartedYet
    }

module ScoreCalculator =

    let private updateScoreState currentScoreState roll =
        match roll, currentScoreState with
        | RollResult.Strike, ScoreState.NextAndFollowing(_, bonus2) ->
            ScoreState.NextAndFollowing((RollResult.Strike :: bonus2), [ RollResult.Strike ])
        | RollResult.Strike, ScoreState.Next _ ->
            ScoreState.NextAndFollowing([ RollResult.Strike ], [ RollResult.Strike ])
        | RollResult.Strike, ScoreState.None ->
            ScoreState.NextAndFollowing([ RollResult.Strike ], [ RollResult.Strike ])
        | RollResult.Spare, ScoreState.NextAndFollowing(_, bonus2) -> ScoreState.Next(RollResult.Spare :: bonus2)
        | RollResult.Spare, ScoreState.Next _ -> ScoreState.Next([ RollResult.Spare ])
        | RollResult.Spare, ScoreState.None -> ScoreState.Next([ RollResult.Spare ])
        | RollResult.Open, ScoreState.NextAndFollowing(_, bonus2) -> ScoreState.Next(bonus2)
        | RollResult.Open, ScoreState.Next _ -> ScoreState.None
        | RollResult.Open, ScoreState.None -> ScoreState.None

    let private updateGameState currentGameState fallenPins =

        let nextFrame rollResult (currentFrameId: int<frame>) =
            match currentFrameId, rollResult with
            | 10<frame>, RollResult.Strike -> GameState.NeedsExtraRolls 2
            | 10<frame>, RollResult.Spare -> GameState.NeedsExtraRolls 1
            | 10<frame>, RollResult.Open -> GameState.GameFinished
            | _ -> GameState.FrameFinished(currentFrameId)

        let firstRollForFrame previousFrameId =
            let newFrameId =
                match previousFrameId with
                | None -> 1<frame>
                | Some id -> id + 1<frame>

            match fallenPins with
            | 10<fallenPins> -> (nextFrame RollResult.Strike newFrameId), RollResult.Strike
            | c -> GameState.OpenFrameStarted(c, newFrameId), RollResult.Open

        match currentGameState with
        | GameState.NotStartedYet -> firstRollForFrame None
        | GameState.FrameFinished previousFrameId -> firstRollForFrame (Some previousFrameId)
        | GameState.OpenFrameStarted(fallenPinsAtFirstRoll, currentFrameId) ->
            match (fallenPins + fallenPinsAtFirstRoll) with
            | 10<fallenPins> -> nextFrame RollResult.Spare currentFrameId, RollResult.Spare
            | _ -> nextFrame RollResult.Open currentFrameId, RollResult.Open
        | GameState.NeedsExtraRolls count ->
            if count = 1 then
                GameState.GameFinished, RollResult.Open
            else
                GameState.NeedsExtraRolls(count - 1), RollResult.Open
        | GameState.GameFinished -> failwith "Game is already finished"

    let private calculateScore currentState scoreState (fallenPins: int<fallenPins>) =

        let baseMultiplier =
            match currentState with
            | GameState.NeedsExtraRolls _ -> 0
            | _ -> 1

        let multiplier =
            match scoreState with
            | ScoreState.None -> baseMultiplier
            | ScoreState.Next bonus -> baseMultiplier + bonus.Length
            | ScoreState.NextAndFollowing(bonus1, _) -> baseMultiplier + bonus1.Length

        multiplier * fallenPins * 1<point / fallenPins>

    let roll game fallenPins =
        let scoreToAdd = calculateScore game.GameState game.ScoreState fallenPins
        let newGameState, rollResult = updateGameState game.GameState fallenPins
        let newScoreState = updateScoreState game.ScoreState rollResult

        {
            Score = game.Score + scoreToAdd
            ScoreState = newScoreState
            GameState = newGameState
        }

    let calculateTotalScore (scoreSheet: ScoreSheet) =

        let initialState = Game.init ()

        let rolls =
            scoreSheet
            |> List.collect (fun frame ->
                match frame with
                | Strike -> [ 10<fallenPins> ]
                | Spare first -> [
                    first
                    10<fallenPins> - first
                  ]
                | Open(first, second) -> [
                    first
                    second
                  ]
                | Extra(extra) -> [ extra ]
            )

        let finalState = rolls |> List.fold roll initialState

        finalState.Score
