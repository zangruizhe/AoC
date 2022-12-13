module Program


[<EntryPoint>]
let main _ =
    "2022_D13.txt" |> AocInput.GetInputAsText |> AoC2022.Day13.F1 |> printfn "%A"
    "2022_D13.txt" |> AocInput.GetInputAsText |> AoC2022.Day13.F2 |> printfn "%A"

    0
