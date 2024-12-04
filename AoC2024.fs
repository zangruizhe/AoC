open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

type Index = int * int
type ll = int64
type ull = uint64
type pii = int * int
type pll = int64 * int64
type vi = int[]
type vvi = int[][]
type vvvi = int[][][]

type vb = bool[]
type vvb = bool[][]
type vvvb = bool[][][]

let IMIN = Int32.MinValue
let IMAX = Int32.MaxValue
let DMAX = Double.MaxValue
let DMIN = Double.MinValue
let DNAN = Double.NaN
let pi: double = 3.14159265358979323846

let tmp_dict = Dictionary<int, int>()
let tmp_set = HashSet<int>()

let RedirectConsole () =
    let if_stream = new FileStream("test.in", FileMode.OpenOrCreate, FileAccess.Read)
    let sin = new StreamReader(if_stream)
    Console.SetIn(sin)
    
    // let of_stream = new FileStream("test.out", FileMode.Create, FileAccess.Write)
    // let sout = new StreamWriter(of_stream)
    // sout.AutoFlush <- true
    // Console.SetOut(sout)

let ReadInputV2 () : string[] =
    let rec loop acc =
        let line = Console.ReadLine()
        if line <> null then loop (line :: acc) else acc

    loop [] |> List.rev |> List.toArray

let ReadInput () : string[] =
    let input = ResizeArray<string>()
    let mutable line = Console.ReadLine()

    while line <> null do
        input.Add(line)
        line <- Console.ReadLine()

    input |> Seq.toArray


// let Join<'t> (src: 't []) : string =
//     src |> Array.map string |> String.concat " "

let split2Str (src: string) : string[] = src.Split(" ")
let split2Int (src: string) : int[] = src |> split2Str |> Array.map int
let split2IntByReg (patten: string) (src:string) = 
    let reg = Regex.Matches(src, patten)

    reg
    |> Seq.map (fun m -> m.Groups[1].Value |> int)
    |> Seq.toArray

module Solve =
    let Solution (lines: string[]) =
        let nums = lines |> Array.map (split2IntByReg "(\d+)")
        let l = ResizeArray<int>()
        let r = ResizeArray<int>()
        nums |> Array.iter(fun n ->
                           l.Add(n[0])
                           r.Add(n[1]))
        l.Sort()
        r.Sort()
        (l, r) ||> Seq.map2 (fun l r -> r - l) |> Seq.sum


RedirectConsole()
let start = DateTime.Now
ReadInput() |> Solve.Solution |>(fun x -> printfn $"{x}")
printfn $"Execution time: %A{(DateTime.Now - start).TotalSeconds} seconds"
