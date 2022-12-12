module Program


[<EntryPoint>]
let main _ =
    "2022_D12.txt" |> AocInput.GetInput |> AoC2022.Day12.F1 |> printfn "%A"
    "2022_D12.txt" |> AocInput.GetInput |> AoC2022.Day12.F2 |> printfn "%A"

    0
