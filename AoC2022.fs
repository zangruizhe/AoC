module AoC2022
// https://adventofcode.com/2022

open System
open FsUnit
open Xunit
open Xunit.Abstractions
open System.Collections.Generic

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

module Day3 =
    let GetPrior (c: char) =
        if c >= 'a' then
            int c - int 'a' + 1
        else
            int c - int 'A' + 27

    let F1 (input: string []) : int =
        input
        |> Seq.map (fun s -> s |> Seq.splitInto 2 |> Seq.map Set.ofSeq)
        |> Seq.collect Set.intersectMany
        |> Seq.sumBy GetPrior

    let F2 (input: string []) : int =
        input
        |> Seq.map Set.ofSeq
        |> Seq.chunkBySize 3
        |> Seq.collect Set.intersectMany
        |> Seq.sumBy GetPrior

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

module Day4 =
    let Parse (input: string []) =
        input
        |> Array.map (fun s -> s.Split([| '-'; ',' |]) |> Array.map int)

    let F1 (input: string []) : int =
        input
        |> Parse
        |> Array.sumBy (fun num ->
            if (num[0] <= num[2] && num[1] >= num[3])
               || (num[2] <= num[0] && num[3] >= num[1]) then
                1
            else
                0)

    let F2 (input: string []) : int =
        input
        |> Parse
        |> Array.sumBy (fun num ->
            if num[2] <= num[1] && num[3] >= num[0] then
                1
            else
                0)


    [<Fact>]
    let ``Day 4`` () =
        "2022_D4.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 487

        "2022_D4.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 849

module Day5 =
    let ParseStack (input: string []) =
        let stack =
            Array.init 9 (fun _ -> List.empty)

        input
        |> Array.map (Seq.chunkBySize 4)
        |> Array.iter (fun s ->
            s
            |> Seq.iteri (fun i v ->
                if v[1] <> ' ' then
                    stack[i] <- stack[i] @ [ v[1] ]))

        stack

    let ParseCommand (input: string []) =
        input
        |> Array.map (fun s ->
            s.Split(' ')
            |> (fun x -> int x[1], int x[3] - 1, int x[5] - 1))

    let ExeCommand (input: string []) take_f =
        let split_i =
            input |> Array.findIndex (fun s -> s.Length = 0)

        let stack =
            input[.. split_i - 2] |> ParseStack

        let command =
            input[split_i + 1 ..] |> ParseCommand

        command
        |> Array.iter (fun (v, f, t) ->
            let take = stack[f] |> List.take v |> take_f
            stack[f] <- stack[f] |> List.skip v
            stack[t] <- take @ stack[t])

        stack
        |> Array.choose (fun s ->
            match s with
            | h :: _ -> Some h
            | [] -> None)
        |> String

    let F1 (input: string []) : string = ExeCommand input List.rev

    let F2 (input: string []) : string = ExeCommand input id

    [<Fact>]
    let ``Day 5`` () =
        "2022_D5.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal "FJSRQCFTN"

        "2022_D5.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal "CJVLJQPHS"

module Day6 =
    let GetIndex (input: string []) (req: int) : int =
        let s = input.[0]
        let mem = Dictionary<char, int>()

        let rec Window l r =
            if r >= s.Length then
                r
            else if r - l = req then
                if mem.Count = req then
                    r
                else
                    mem[s[l]] <- mem[s[l]] - 1

                    if mem[s[l]] = 0 then
                        mem.Remove(s[l]) |> ignore

                    Window (l + 1) r
            else
                let c = s[r]

                match mem.ContainsKey(c) with
                | true -> mem[c] <- mem[c] + 1
                | false -> mem.Add(c, 1)

                Window l (r + 1)

        Window 0 0

    let F1 (input: string []) : int = GetIndex input 4

    let F2 (input: string []) : int =
        let i1 = GetIndex input 4

        i1
        + GetIndex (input.[0][i1..] |> Array.singleton) 14

    [<Fact>]
    let ``Day 6`` () =
        "2022_D6.txt"
        |> AocInput.GetInput
        |> F1
        |> should equal 1578

        "2022_D6.txt"
        |> AocInput.GetInput
        |> F2
        |> should equal 2178
