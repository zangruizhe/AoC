module Program

open System.IO
open System

[<EntryPoint>]
let main _ =

    let of_stream =
        new FileStream("local.out", FileMode.Create, FileAccess.Write)

    let sout = new StreamWriter(of_stream)
    do sout.AutoFlush <- true

    do Console.SetOut(sout)

    printfn "1111111111"
    0
