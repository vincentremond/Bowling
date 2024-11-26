module Bowling.Tests

open NUnit.Framework
open Bowling

let (|CharAsInt|_|) c =
    match c with
    | '-' -> Some 0<fallenPins>
    | '1' -> Some 1<fallenPins>
    | '2' -> Some 2<fallenPins>
    | '3' -> Some 3<fallenPins>
    | '4' -> Some 4<fallenPins>
    | '5' -> Some 5<fallenPins>
    | '6' -> Some 6<fallenPins>
    | '7' -> Some 7<fallenPins>
    | '8' -> Some 8<fallenPins>
    | '9' -> Some 9<fallenPins>
    | _ -> None

let parseTextSheet (str: string) : ScoreSheet =
    str
    |> List.ofSeq
    |> List.windowedStep 4 3
    |> List.map (fun chars ->
        match chars with
        | [ '|' ; 'X' ; ' ' ; '|' ] -> Strike
        | [ '|' ; CharAsInt first ; '/' ; '|' ] -> Spare first
        | [ '|' ; CharAsInt first ; CharAsInt second ; '|' ] -> Open(first, second)
        | [ '|' ; CharAsInt extra ; ' ' ; '|' ] -> Extra(extra)
        | f -> failwith $"Invalid frame {f}"
    )
    

[<Test>]
let ``Test multiple sheets`` () =
    
    let scoreSheets = [
        "|X |X |X |X |X |X |X |X |X |X |X |X |", 300
        "|X |X |X |X |X |X |X |X |X |X |X |9 |", 299
        "|5/|5/|5/|5/|5/|5/|5/|5/|5/|5/|5 |", 150
        "|9-|9-|9-|9-|9-|9-|9-|9-|9-|9-|", 90
    ]
    
    for scoreSheet, expected in scoreSheets do
        
        let scores = parseTextSheet scoreSheet
        let result = ScoreCalculator.calculateTotalScore scores
        
        Assert.That(result, Is.EqualTo(expected))
    