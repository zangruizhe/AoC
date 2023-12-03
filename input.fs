module AocInput

open System.IO
open Xunit.Abstractions

let XLOG x =
    printfn $"%A{x}"
    x

let LOG x = printfn $"%A{x}"

type Converter(output: ITestOutputHelper) =
    inherit TextWriter()
    override _.Encoding = stdout.Encoding
    override _.WriteLine message = output.WriteLine message
    override _.Write message = output.WriteLine message

let Char2Int (c: char) : int = int c - int '0'

let IntArray (s: string) = s.Split(';') |> Array.map int

let GetInput (file: string) =
    File.ReadLines(__SOURCE_DIRECTORY__ + $"/data/{file}")
    |> Seq.toArray
    |> fun x ->
        printfn $"Read lines={x.Length}"
        x

let GetInputAsText (file: string) =
    let txt = File.ReadAllText(__SOURCE_DIRECTORY__ + $"/data/{file}")

    txt.Remove(txt.Length - 1, 1)
    |> fun x ->
        printfn $"Read string={x.Length}"
        x
