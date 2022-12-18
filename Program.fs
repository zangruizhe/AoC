module Program

open System
open System.IO

let RedirectConsole () =
    let if_stream = new FileStream("local.in", FileMode.OpenOrCreate, FileAccess.Read)

    let of_stream = new FileStream("local.out", FileMode.Create, FileAccess.Write)

    let sin = new StreamReader(if_stream)
    let sout = new StreamWriter(of_stream)
    sout.AutoFlush <- true

    Console.SetIn(sin)
    Console.SetOut(sout)

[<EntryPoint>]
let main _ =
    RedirectConsole()
    "2022_D17.txt" |> AocInput.GetInput |> AoC2022.Day17.F1 |> printfn "%A"
    "2022_D17.txt" |> AocInput.GetInput |> AoC2022.Day17.F2 |> printfn "%A"

    0
