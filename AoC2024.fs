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

let split2Str (split: string) (src: string) : string[] =
    src.Split(split, StringSplitOptions.RemoveEmptyEntries)

let split2Int (src: string) : int[] = src |> split2Str " " |> Array.map int

let splitByReg (patten: string) (src: string) =
    let reg = Regex.Matches(src, patten)
    reg |> Seq.map (_.Groups) |> Seq.toArray

let split2IntByReg (patten: string) (src: string) =
    let rst =
        splitByReg patten src
        |> Array.map (fun g -> g |> Seq.skip 1 |> Seq.map (fun g -> int g.Value) |> Seq.toArray)

    rst

module Day1 =
    let l = ResizeArray<int>()
    let r = ResizeArray<int>()

    let init (lines) =
        lines
        |> Array.map (split2Int)
        |> Array.iter (fun n ->
            l.Add(n[0])
            r.Add(n[1]))

        l.Sort()
        r.Sort()

    let Q1 (lines: string[]) =
        init lines
        (l, r) ||> Seq.map2 (fun l r -> abs (r - l)) |> Seq.sum

    let Q2 (lines: string[]) =
        init lines
        let r_dict = Dictionary<int, int>()
        r |> Seq.iter (fun n -> r_dict[n] <- r_dict.GetValueOrDefault(n) + 1)
        l |> Seq.map (fun n -> n * r_dict.GetValueOrDefault(n)) |> Seq.sum

module Day2 =
    let init (lines: string[]) = lines |> Array.map split2Int

    let check (nums: int[]) =
        nums
        |> Array.pairwise
        |> Array.map (fun (l, r) -> r - l)
        |> fun pair ->
            (pair |> Array.forall (fun n -> 1 <= n && n <= 3)
             || pair |> Array.forall (fun n -> -1 >= n && n >= -3))

    let Q1 (lines: string[]) =
        lines |> init |> Array.filter check |> Array.length

    let Q2 (lines: string[]) =
        let lines = lines |> init

        let bad_lines =
            lines
            |> Array.filter (fun nums -> not (check nums))
            |> Array.filter (fun nums ->
                [| 0 .. nums.Length - 1 |]
                |> Array.exists (fun i ->
                    let new_nums = Array.removeAt i nums
                    check new_nums)
                |> not)

        lines.Length - bad_lines.Length

module Day3 =
    let Q1 (lines: string[]) =
        let str = String.Concat(lines)
        str |> split2IntByReg "mul\((\d+),(\d+)\)" |> Array.sumBy (fun n -> n[0] * n[1])

    let Q2 (lines: string[]) =
        let str = String.Concat(lines)

        Regex.Matches(str, @"mul\((\d+),(\d+)\)|don't\(\)|do\(\)")
        |> Seq.fold
            (fun (l, isEnable) m ->
                if m.Value.StartsWith("mul") then
                    if isEnable then
                        l @ [ (int m.Groups[1].Value, int m.Groups[2].Value) ], isEnable
                    else
                        l, isEnable
                elif m.Value = "do()" then
                    l, true
                elif m.Value = "don't()" then
                    l, false
                else
                    failwith $"wrong patten m={m.Value}")
            ([], true)
        |> fst
        |> Seq.sumBy (fun (l, r) -> l * r)

module Day4 =
    let Q1 (lines: string[]) =
        let word = "XMAS"
        let R = lines.Length
        let C = lines[0].Length

        let rec dfs i j n (l, r) =
            if i < 0 || i >= R || j < 0 || j >= C then
                false
            else
                match lines[i][j] = word[n], n = word.Length - 1 with
                | true, true -> true
                | false, _ -> false
                | _ -> dfs (i + l) (j + r) (n + 1) (l, r)


        let check i j =
            [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
            |> List.filter (dfs i j 0)
            |> List.length

        let rst =
            [ for i in 0 .. R - 1 do
                  for j in 0 .. C - 1 do
                      if lines[i][j] = word[0] then
                          check i j
              ]
            |> List.sum

        rst

    let Q2 (lines: string[]) =
        let word = "MAS"
        let R = lines.Length
        let C = lines[0].Length

        let rec dfs i j n (l, r) =
            if i < 0 || i >= R || j < 0 || j >= C then
                false
            else
                match lines[i][j] = word[n], n = word.Length - 1 with
                | true, true -> true
                | false, _ -> false
                | _ -> dfs (i + l) (j + r) (n + 1) (l, r)


        let getMid i j =
            [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
            |> List.filter (dfs i j 0)
            |> List.map (fun (l, r) -> (i + l), (j + r))

        let rst =
            [ for i in 0 .. R - 1 do
                  for j in 0 .. C - 1 do
                      if lines[i][j] = word[0] then
                          yield! getMid i j ]
            |> List.countBy id
            |> List.filter (fun idx -> (snd idx) = 2)
            |> List.length

        rst


module Day =
    let init (lines: string[]) = lines |> Array.map split2Int

    let Q1 (lines: string[]) =
        let lines = init lines
        0

    let Q2 (lines: string[]) =
        let lines = init lines
        0

let start = DateTime.Now
let lines = File.ReadAllLines "test.in"
lines |> Day4.Q1 |> (fun x -> printfn $"Q1={x}")
lines |> Day4.Q2 |> (fun x -> printfn $"Q2={x}")
printfn $"Execution time: %A{(DateTime.Now - start).TotalSeconds} seconds"
