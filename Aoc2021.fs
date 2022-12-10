module Aoc2021
// https://adventofcode.com/2021

open Xunit
open FsUnit

module Day1 =
    let F1 (nums: int []) =
        nums
        |> Array.pairwise
        |> Array.filter (fun (l, r) -> r > l)
        |> Array.length

    let F2 (nums: int []) =
        nums
        |> Array.windowed 3
        |> Array.map (fun v -> v |> Array.sum)
        |> Array.pairwise
        |> Array.filter (fun (l, r) -> r > l)
        |> Array.length

[<Fact>]
let ``Day 1`` () =
    AocInput.Aoc2021.D1
    |> AocInput.IntArray
    |> Day1.F1
    |> should equal 1655

    AocInput.Aoc2021.D1
    |> AocInput.IntArray
    |> Day1.F2
    |> should equal 1683

type OP =
    | Forward of int
    | Down of int
    | Up of int

type Position = { Horizontal: int; Depth: int; Aim: int }

let GetOP (op: string) (v: int) =
    match op with
    | "forward" -> Forward v
    | "down" -> Down v
    | "up" -> Up v
    | _ -> failwith $"can not get here %s{op}"

let ApplyOP (op: OP) (pos: Position) =
    match op with
    | Forward i ->
        { pos with Horizontal = pos.Horizontal + i ; Depth = pos.Depth + pos.Aim * i}
    | Up i -> { pos with Aim = pos.Aim - i }
    | Down i -> { pos with Aim = pos.Aim + i }

module Day2 =
    let F2 (ops: OP []) =
        let pos = { Horizontal = 0; Depth = 0 ; Aim = 0}

        ops
        |> Array.fold (fun state op -> ApplyOP op state) pos
        |> fun pos -> pos.Depth * pos.Horizontal


[<Fact>]
let ``Day 2`` () =
    AocInput.Aoc2021.D2
    |> (fun s -> s.Split(';'))
    |> Array.map (fun s ->
        let t = s.Split(' ')
        GetOP t[0] (int t[1]))
    |> Day2.F2
    |> should equal 1544000595

    "forward 5 ;down 5 ;forward 8 ;up 3 ;down 8 ;forward 2"
    |> (fun s -> s.Split(';'))
    |> Array.map (fun s ->
        let t = s.Split(' ')
        GetOP t[0] (int t[1]))
    |> Day2.F2
    |> should equal 900

[<Fact>]
let ``My test`` () = 0 |> should equal 0
