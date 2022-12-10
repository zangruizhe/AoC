module Program


[<EntryPoint>]
let main _ =
    "2022_D9.txt"
    |> AocInput.GetInput
    |> AoC2022.Day9.F2
    |> printfn "%A"

    0
