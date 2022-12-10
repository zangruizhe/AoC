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
        |> F1
        |> should equal 1655

        AocInput.Aoc2021.D1
        |> AocInput.IntArray
        |> F2
        |> should equal 1683

type OP =
    | Forward of int
    | Down of int
    | Up of int

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }

let GetOP (op: string) (v: int) =
    match op with
    | "forward" -> Forward v
    | "down" -> Down v
    | "up" -> Up v
    | _ -> failwith $"can not get here %s{op}"

let ApplyOP (op: OP) (pos: Position) =
    match op with
    | Forward i ->
        { pos with
            Horizontal = pos.Horizontal + i
            Depth = pos.Depth + pos.Aim * i }
    | Up i -> { pos with Aim = pos.Aim - i }
    | Down i -> { pos with Aim = pos.Aim + i }

module Day2 =
    let F2 (ops: OP []) =
        let pos =
            { Horizontal = 0; Depth = 0; Aim = 0 }

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
        |> F2
        |> should equal 1544000595

        "forward 5 ;down 5 ;forward 8 ;up 3 ;down 8 ;forward 2"
        |> (fun s -> s.Split(';'))
        |> Array.map (fun s ->
            let t = s.Split(' ')
            GetOP t[0] (int t[1]))
        |> F2
        |> should equal 900

module Day3 =
    let GetNum (s: string) =
        [ s.Length - 1 .. -1 .. 0 ]
        |> List.fold
            (fun (num, b) i ->
                match s[i] = '1' with
                | true -> (num + b), b * 2
                | false -> num, b * 2)
            (0, 1)
        |> fst

    let F1 (input: string []) : int =
        let mem = Array.zeroCreate input[0].Length

        input
        |> Array.iter (fun l ->
            l.ToCharArray()
            |> Array.iteri (fun i v -> if v = '1' then mem[i] <- mem[i] + 1))

        let gamma =
            mem
            |> Array.fold
                (fun s v ->
                    match v > input.Length / 2 with
                    | true -> s + "1"
                    | false -> s + "0")
                ""

        let epsilon =
            gamma.ToCharArray()
            |> Array.fold
                (fun s v ->
                    match v = '1' with
                    | true -> s + "0"
                    | false -> s + "1")
                ""


        (GetNum gamma) * (GetNum epsilon)

    let F2 (input: string []) : int =
        let rec ArrFilter f i (nums: string []) =
            if i = input[0].Length || Array.length nums = 1 then
                nums[0]
            else
                let key =
                    nums
                    |> Array.fold (fun n l -> if l[i] = '1' then n + 1 else n) 0
                    |> (fun v ->
                        match v * 2 >= nums.Length with
                        | true -> '1'
                        | false -> '0')

                nums
                |> Array.filter (fun v -> f v[i] key)
                |> ArrFilter f (i + 1)

        let oxygen = ArrFilter (=) 0 input |> GetNum
        let CO2 = ArrFilter (<>) 0 input |> GetNum

        oxygen * CO2

    [<Fact>]
    let ``Day 3`` () =
        "2021_D3.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 3549854


        //        [| "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010" |]
        "2021_D3.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 3765399

module Day4 =
    let F1 (input: string []) : int =
        let nums =
            input[ 0 ].Split(",") |> Array.map int

        let boards: (Set<int> * int) [] [] =
            [| 2..6 .. input.Length - 1 |]
            |> Array.map (fun i ->
                [| i .. i + 4 |]
                |> Array.map (fun j ->
                    input[ j ].Split(" ")
                    |> Array.filter (fun s -> s.Length <> 0)
                    |> Array.map int
                    |> Set.ofArray
                    |> fun x -> x, x.Count))

        let rec Play i j v : int option =
            if i >= boards.Length then
                None
            elif j >= boards[0].Length then
                Play (i + 1) 0 v
            else
                let l, n = boards[i][j]

                match Set.contains v l with
                | true ->
                    let next_l = Set.remove v l
                    let next = n - 1
                    boards[i][j] <- next_l, next

                    if next = 0 then
                        boards[i]
                        |> Array.sumBy (fun (l, _) -> l |> Set.toArray |> Array.sum)
                        |> (*) v
                        |> Some
                    else
                        Play i (j + 1) v
                | false -> Play i (j + 1) v

        let mutable rst = 0

        nums
        |> Array.exists (fun v ->
            match Play 0 0 v with
            | Some n ->
                rst <- n
                true
            | None -> false)
        |> ignore

        rst

    let F2 (input: string []) : int =
        let nums =
            input[ 0 ].Split(",") |> Array.map int

        let boards: int [] [] [] =
            [| 2..6 .. input.Length - 1 |]
            |> Array.map (fun i ->
                [| i .. i + 4 |]
                |> Array.map (fun j ->
                    input[ j ].Split(" ")
                    |> Array.filter (fun s -> s.Length <> 0)
                    |> Array.map int
                    |> fun x -> Array.append x [| x.Length |])
                |> fun x -> Array.append x [| Array.init x.Length (fun _ -> x.Length) |])


        let win =
            Array.init boards.Length (fun _ -> false)

        let len = (boards[0][0]).Length - 1

        let rec Play i j v : int option =
            if i >= boards.Length then
                None
            elif j >= boards[0].Length - 1 then
                Play (i + 1) 0 v
            else
                match win[i] with
                | true -> Play (i + 1) 0 v
                | false ->
                    let l = boards[i][j]

                    match Array.tryFindIndex ((=) v) l[0 .. len - 1] with
                    | Some k ->
                        boards[i].[j][k] <- -1
                        boards[i].[j][len] <- boards[i].[j][len] - 1
                        boards[i].[len][k] <- boards[i].[len][k] - 1

                        if boards[i].[j][len] = 0 || boards[i].[len][k] = 0 then
                            win[i] <- true

                            if (win |> Array.filter id |> Array.length) = boards.Length then
                                boards[i][0 .. len - 1]
                                |> Array.sumBy (fun l ->
                                    l[0 .. len - 1]
                                    |> Array.filter ((<>) -1)
                                    |> Array.sum)
                                |> (*) v
                                |> Some
                            else
                                Play (i + 1) 0 v
                        else
                            Play i (j + 1) v
                    | None -> Play i (j + 1) v

        let mutable rst = 0

        nums
        |> Array.exists (fun v ->
            match Play 0 0 v with
            | Some n ->
                rst <- n
                true
            | None -> false)
        |> ignore

        rst


    [<Fact>]
    let ``Day 4`` () =
        "2021_D4.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 22680

        "2021_D4.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 16168

module DayN =
    let F1 (input: string []) : int = 0

    [<Fact>]
    let ``Day N`` () =
        //        "2021_D3.txt"
//        |> AocInput.GetInput
        [||] |> F1 |> should equal 0
