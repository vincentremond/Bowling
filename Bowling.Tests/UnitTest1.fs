module Bowling.Tests

open NUnit.Framework
open Bowling

let (|CharAsInt|_|) c =
    match c with
    | '-' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
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
let Test1 () =
    
    let scoreSheets = [
        "|9-|9-|9-|9-|9-|9-|9-|9-|9-|9-|", 90
        "|5/|5/|5/|5/|5/|5/|5/|5/|5/|5/|5 |", 150
        "|X |X |X |X |X |X |X |X |X |X |X |X |", 300
    ]
    
    for scoreSheet, expected in scoreSheets do
        
        let scores = parseTextSheet scoreSheet
        let result = ScoreCalculator.calculateScore scores
        
        Assert.That(result, Is.EqualTo(expected))
    