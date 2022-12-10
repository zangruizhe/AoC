module AoC2022
// https://adventofcode.com/2022

open System
open FsUnit
open Xunit
open Xunit.Abstractions

// module Day1 =
type Day1(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let rec Acc (input: string []) (acc: int []) i : int [] =
        if i = input.Length then
            acc
        else if input[i].Length = 0 then
            Acc input (Array.append acc [| 0 |]) (i + 1)
        else
            acc[acc.Length - 1] <- acc[acc.Length - 1] + int (input[i])
            Acc input acc (i + 1)

    let F1 (input: string []) : int = Acc input [| 0 |] 0 |> Array.max

    let F2 (input: string []) : int =
        Acc input [| 0 |] 0
        |> Array.sortByDescending id
        |> Array.take 3
        |> Array.sum

    [<Fact>]
    let ``Day 1`` () =
        "2022_D1.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 72511

        "2022_D1.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 212117

type GameResult =
    | Win
    | Lost
    | Draw

type GameOP =
    | Rock
    | Paper
    | Scissor
    static member Play (my: GameOP) (other: GameOP) : int =
        let win = 6
        let draw = 3
        let lost = 0

        let rock = 1
        let paper = 2
        let scissor = 3

        match my, other with
        | Rock, Scissor -> rock + win
        | Rock, Paper -> rock + lost
        | Rock, Rock -> rock + draw

        | Paper, Scissor -> paper + lost
        | Paper, Paper -> paper + draw
        | Paper, Rock -> paper + win

        | Scissor, Scissor -> scissor + draw
        | Scissor, Paper -> scissor + win
        | Scissor, Rock -> scissor + lost

    static member ReversePlay (rst: GameResult) (other: GameOP) : int =
        let win = 6
        let draw = 3
        let lost = 0

        let rock = 1
        let paper = 2
        let scissor = 3

        match rst, other with
        | Win, Scissor -> rock + win
        | Win, Paper -> scissor + win
        | Win, Rock -> paper + win

        | Lost, Scissor -> paper + lost
        | Lost, Paper -> rock + lost
        | Lost, Rock -> scissor + lost

        | Draw, Scissor -> scissor + draw
        | Draw, Paper -> paper + draw
        | Draw, Rock -> rock + draw

type Day2(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let parse_other a =
        if a = 'A' then Rock
        elif a = 'B' then Paper
        else Scissor

    let parse_my_op a =
        if a = 'X' then Rock
        elif a = 'Y' then Paper
        else Scissor

    let parse_my_rst a =
        if a = 'X' then Lost
        elif a = 'Y' then Draw
        else Win

    let F1 (input: string []) : int =
        input
        |> Array.map (fun l -> (parse_my_op l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameOP.Play my other)
        |> Array.sum

    let F2 (input: string []) : int =
        input
        |> Array.map (fun l -> (parse_my_rst l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameOP.ReversePlay my other)
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
