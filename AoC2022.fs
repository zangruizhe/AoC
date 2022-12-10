module AoC2022
// https://adventofcode.com/2022

open System
open FsUnit
open Xunit
open Xunit.Abstractions

type Day1(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let GetCal (input: string) : int [] =
        input.Split("\n\n")
        |> Array.map (fun s -> s.Split("\n") |> Seq.sumBy int)

    let F1 (input: string) : int = input |> GetCal |> Array.max

    let F2 (input: string) : int =
        input
        |> GetCal
        |> Array.sortByDescending id
        |> Array.take 3
        |> Array.sum

    [<Fact>]
    let ``Day 1`` () =
        "2022_D1.txt"
        |> AocInput.GetInputAsText
        |> F1
        |> should equal 72511

        "2022_D1.txt"
        |> AocInput.GetInputAsText
        |> F2
        |> should equal 212117


module Day2 =
    type GameResult =
        | Win
        | Lost
        | Draw

    type GameOP =
        | Rock
        | Paper
        | Scissor

    let ChoiceScore op =
        match op with
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

    let parse_other a =
        if a = 'A' then Rock
        elif a = 'B' then Paper
        else Scissor

    let parse_my_op a =
        if a = 'X' then Rock
        elif a = 'Y' then Paper
        else Scissor

    let F1 (input: string []) : int =
        let GameScore (my: GameOP) (other: GameOP) : int =
            match my, other with
            | Rock, Scissor
            | Paper, Rock
            | Scissor, Paper -> 6

            | Rock, Paper
            | Paper, Scissor
            | Scissor, Rock -> 0

            | Rock, Rock
            | Paper, Paper
            | Scissor, Scissor -> 3

        input
        |> Array.map (fun l -> (parse_my_op l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameScore my other + ChoiceScore my)
        |> Array.sum

    let F2 (input: string []) : int =
        let GameScore (rst: GameResult) (other: GameOP) : int =
            match rst, other with
            | Win, Scissor
            | Lost, Paper
            | Draw, Rock -> 1

            | Win, Rock
            | Lost, Scissor
            | Draw, Paper -> 2

            | Win, Paper
            | Lost, Rock
            | Draw, Scissor -> 3

        let parse_my_rst a =
            if a = 'X' then Lost
            elif a = 'Y' then Draw
            else Win

        let ResultScore rst =
            match rst with
            | Win -> 6
            | Lost -> 0
            | Draw -> 3

        input
        |> Array.map (fun l -> (parse_my_rst l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameScore my other + ResultScore my)
        |> Array.sum

    [<Fact>]
    let ``Day 2`` () =
        "2022_D2.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 11603

        "2022_D2.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 12725

type Day3(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let GetPrior (c: char) =
        if c >= 'a' then
            int c - int 'a' + 1
        else
            int c - int 'A' + 27

    let F1 (input: string []) : int =
        let GetShareItem (l: string) (r: string) =
            let ls = Set.ofSeq l
            let rs = Set.ofSeq r
            Set.intersect ls rs |> Set.toArray |> Array.head

        input
        |> Array.map (fun s ->
            GetShareItem s[0 .. s.Length / 2 - 1] s[s.Length / 2 ..]
            |> GetPrior)
        |> Array.sum

    let F2 (input: string []) : int =
        let GetShareItem (l: string) (r: string) (m: string) =
            (Set.ofSeq r)
            |> Set.intersect (Set.ofSeq l)
            |> Set.intersect (Set.ofSeq m)
            |> Set.toArray
            |> Array.head

        input
        |> Array.splitInto (input.Length/3)
        |> Array.map (fun s -> GetShareItem s[0] s[1] s[2] |> GetPrior)
        |> Array.sum

    [<Fact>]
    let ``Day 3`` () =
        "2022_D3.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 7742

        "2022_D3.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 2276
