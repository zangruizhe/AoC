module Program


[<EntryPoint>]
let main _ =
    "2022_D11.txt" |> AocInput.GetInputAsText |> AoC2022.Day11.F1 |> printfn "%A"
    "2022_D11.txt" |> AocInput.GetInputAsText |> AoC2022.Day11.F2 |> printfn "%A"

    0
