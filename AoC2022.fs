module AoC2022
// https://adventofcode.com/2022

open Xunit
open FsUnit
open Xunit.Abstractions
open System

// module Day1 =
type Day1(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let rec Acc (input:string []) (acc: int []) i : int [] =
            if i = input.Length then
                acc
            else if input[i].Length = 0 then
                Acc input (Array.append acc [| 0 |]) (i + 1)
            else
                acc[acc.Length - 1] <- acc[acc.Length - 1] + int (input[i])
                Acc input acc (i + 1)

    let F1 (input: string []) : int =
        Acc input [| 0 |] 0 |> Array.max

    let F2 (input: string []) : int =
        Acc input [| 0 |] 0 |> Array.sortByDescending id |> Array.take 3 |> Array.sum

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
