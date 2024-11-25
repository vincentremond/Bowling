namespace Bowling

type Frame =
    | Strike
    | Spare of int
    | Open of int * int
    | Extra of int

type ScoreSheet = Frame list

module ScoreCalculator =

    let frameScore =
        function
        | Strike -> 10
        | Spare(firstRoll) -> 10
        | Open(firstRoll, secondRoll) -> firstRoll + secondRoll
        | Extra extra -> extra

    let frameRange =
        function
        | Strike -> 2
        | Spare _ -> 1
        | Open _ -> 0
        | Extra _ -> 0

    let calculateScore (scoreSheet: ScoreSheet) =
        scoreSheet
        |> List.take 10
        |> List.sumByI (fun frameIndex frame ->
            let range = frameRange frame

            [ 0..range ]
            |> List.sumBy (fun offset -> frameScore scoreSheet[frameIndex + offset])
        )
