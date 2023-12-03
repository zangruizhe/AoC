module AoC2023

open System.Text.RegularExpressions

module Day3 =
    let parse input =
        let rex = Regex(@"(\d+) (\w+)")

        input
        |> Array.map (fun (str: string) ->
            str.Split(';')
            |> Seq.map (fun s ->
                let ma = rex.Matches(s)

                ma
                |> Seq.map (fun m -> int (m.Groups[1].Value), m.Groups[2].Value)
                |> Seq.toArray)
            |> Seq.toArray)

    let Q1 (input: string[]) =
        let bags = [ ("red", 12); ("green", 13); ("blue", 14) ] |> Map.ofList
        let parsed = parse input

        parsed
        |> Array.indexed
        |> Array.choose (fun (i, arr) ->
            arr
            |> Array.forall (fun sub_test -> sub_test |> Array.forall (fun (n, color) -> n <= bags[color]))
            |> (fun v ->
                match v with
                | true -> Some(i + 1)
                | false -> None))
        |> Array.sum

    let Q2 (input: string[]) =
        let parsed = parse input
        let base_bag = [ ("red", 0); ("green", 0); ("blue", 0) ] |> Map.ofList

        parsed
        |> Array.map (fun game ->
            game
            |> Seq.fold
                (fun update_bag one_round ->
                    one_round
                    |> Seq.fold (fun bag (n, color) -> Map.add color (max n bag[color]) bag) update_bag)
                base_bag
            |> (fun m -> m.Values |> Seq.reduce (*)))
        |> Array.sum
