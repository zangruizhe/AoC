open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

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

let char2Int (c: char) = int c - int '0'
let int2Char (i: int) = Convert.ToChar i

let split2Str (split: string) (src: string) : string[] =
    src.Split(split, StringSplitOptions.RemoveEmptyEntries)

let split2Int (src: string) : int[] = src |> split2Str " " |> Array.map int

let split2IntBySplit (split: string) (src: string) : int[] = src |> split2Str split |> Array.map int

let splitByReg (patten: string) (src: string) =
    let reg = Regex.Matches(src, patten)
    reg |> Seq.map (_.Groups) |> Seq.toArray

let split2IntByReg (patten: string) (src: string) =
    let rst =
        splitByReg patten src
        |> Array.map (fun g -> g |> Seq.skip 1 |> Seq.map (fun g -> int64 g.Value) |> Seq.toArray)

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
                          check i j ]
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

module Day5 =
    let init (lines: string[]) =
        let i = lines |> Array.findIndex (fun s -> s.Length = 0)
        let rules = lines[.. i - 1]
        let updates = lines[i + 1 ..] |> Array.map (split2IntBySplit ",")
        let rules_dict = Dictionary<int, HashSet<int>>()

        rules
        |> Array.iter (fun s ->
            s
            |> split2IntBySplit "|"
            |> fun nums ->
                let b = nums[0]

                if not (rules_dict.ContainsKey(b)) then
                    rules_dict[b] <- HashSet()

                rules_dict[b].Add(nums[1]) |> ignore)

        rules_dict, updates

    let isGood (rules_dict: Dictionary<int, HashSet<int>>) nums =
        let tmp_set = HashSet<int>()

        nums
        |> Array.exists (fun n ->
            tmp_set.Add(n) |> ignore
            tmp_set.Overlaps(rules_dict.GetValueOrDefault(n, HashSet<int>())))
        |> not

    let Q1 (lines: string[]) =
        let rules_dict, updates = init lines
        let good = updates |> Array.filter (isGood rules_dict)
        good |> Array.sumBy (fun nums -> nums[nums.Length / 2])

    let Q2 (lines: string[]) =
        let rules_dict, updates = init lines

        let rec fix_bad (nums: vi) (rst: int list) =
            if nums.Length = 0 then
                rst
            else
                let next_i =
                    nums
                    |> Array.findIndex (fun n ->
                        nums
                        |> Array.except [ n ]
                        |> Array.exists (fun other -> rules_dict.GetValueOrDefault(other, HashSet<int>()).Contains(n))
                        |> not)

                fix_bad (nums |> Array.removeAt next_i) (nums[next_i] :: rst)


        let bad = updates |> Array.filter (fun nums -> isGood rules_dict nums |> not)

        bad
        |> Array.map (fun nums -> fix_bad nums [])
        |> Array.sumBy (fun nums -> nums[nums.Length / 2])


type Day6(lines: string[]) =
    let R = lines.Length
    let C = lines[0].Length

    let getStart (lines: string[]) =
        lines
        |> Array.indexed
        // |> Array.pick (fun (i, s) -> s |> Seq.tryFindIndex ((=) '^') |> Option.map (fun j -> (i, j)))
        |> Array.pick (fun (i, s) ->
            let j = s.IndexOf("^")
            if j <> -1 then Some(i, j) else None)

    let ops = [ (-1, 0); (0, 1); (1, 0); (0, -1) ]

    let rec moving i j op (pos_set: HashSet<Index>) =
        pos_set.Add((i, j)) |> ignore
        let next_i = i + fst ops[op]
        let next_j = j + snd ops[op]

        if next_i < 0 || next_i >= R || next_j < 0 || next_j >= C then
            pos_set
        elif lines[next_i][next_j] = '#' then
            moving i j ((op + 1) % 4) pos_set
        else
            moving next_i next_j op pos_set


    member this.Q1() =
        let start: Index = getStart lines
        let post_set = moving (fst start) (snd start) 0 (HashSet())
        post_set.Count

    member this.Q2() =
        let start: Index = getStart lines
        let pos_set = moving (fst start) (snd start) 0 (HashSet())
        pos_set.Remove(start) |> ignore
        let isInLoop (pos_set: HashSet<int * int * int>) i j op = pos_set.Add((i, j, op)) = false

        let rec checkLoop i j op (pos_set: HashSet<int * int * int>) =

            let next_i = i + fst ops[op]
            let next_j = j + snd ops[op]

            if next_i < 0 || next_i >= R || next_j < 0 || next_j >= C then
                false
            elif lines[next_i][next_j] = '#' then
                if isInLoop pos_set i j op then
                    true
                else
                    checkLoop i j ((op + 1) % 4) pos_set
            else
                checkLoop next_i next_j op pos_set


        pos_set
        |> Seq.filter (fun (i, j) ->
            let old = lines[i]
            let str = old.ToCharArray()
            str[j] <- '#'
            lines[i] <- String(str)
            let rst = checkLoop (fst start) (snd start) 0 (HashSet())
            lines[i] <- old
            rst)
        |> Seq.length


type Day7(lines: string[]) =
    member this.Q1() =
        let canEvaluated (nums: int64 array) =
            let target = nums[0]
            let nums = nums[1..] |> List.ofArray

            let rec check nums cur =
                if cur > target then
                    false
                else
                    match nums with
                    | [] -> cur = target
                    | n :: t -> check t (n + cur) || check t (n * cur)

            check (List.tail nums) (List.head nums)


        lines
        |> Array.map (split2IntByReg @"(\d+)" >> Array.concat)
        |> Array.filter canEvaluated
        |> Array.sumBy (fun nums -> nums[0])

    member this.Q2() =
        let canEvaluated (nums: int64 array) =
            let target = nums[0]
            let nums = nums[1..] |> List.ofArray

            let rec check nums cur =
                if cur > target then
                    false
                else
                    match nums with
                    | [] -> cur = target
                    | n :: t -> check t (n + cur) || check t (n * cur) || check t ($"{cur}{n}" |> int64)

            check (List.tail nums) (List.head nums)


        lines
        |> Array.map (split2IntByReg @"(\d+)" >> Array.concat)
        |> Array.filter canEvaluated
        |> Array.sumBy (fun nums -> nums[0])

type Day8(lines: string[]) =
    let R = lines.Length
    let C = lines[0].Length

    let getPoints (lines: string[]) : Map<char, Index array> =
        let points =
            [| for i in 0 .. R - 1 do
                   for j in 0 .. C - 1 do
                       if lines[i][j] <> '.' then
                           (lines[i][j], (i, j)) |]

        points
        |> Array.groupBy fst
        |> Map.ofArray
        |> Map.map (fun _ v -> v |> Array.map snd)


    let inBoard (i, j) : bool = 0 <= i && i < R && 0 <= j && j < C

    let getIdxPair (idx_list: Index array) =
        [ for i in 0 .. idx_list.Length - 2 do
              for j in i + 1 .. idx_list.Length - 1 do
                  (idx_list[i], idx_list[j]) ]

    let getAntinodes op (a: Index) (b: Index) : Index list =
        let ar, ac = a
        let br, bc = b

        let x = abs (ar - br)
        let y = abs (ac - bc)

        match ar < br, ac < bc with
        | true, true -> (-x, -y), (x, y)
        | true, false -> (-x, y), (x, -y)
        | false, true -> (x, -y), (-x, y)
        | false, false -> (x, y), (-x, -y)
        |> fun (d1, d2) -> op a d1 @ op b d2

    member this.Q1() =
        let points_dict = lines |> getPoints
        let getAntIdx (ar, ac) (dr, dc) = [ (ar + dr, ac + dc) ]

        points_dict.Values
        |> Array.ofSeq
        |> Seq.collect (fun idx_list ->
            idx_list
            |> getIdxPair
            |> List.collect (fun (l, r) -> getAntinodes getAntIdx l r))
        |> Seq.filter inBoard
        |> Set.ofSeq
        |> Seq.length

    member this.Q2() =
        let points_dict = lines |> getPoints

        let getAntinodesInLine (ar, ac) (x, y) =
            let rec loop (ar, ac) x y rst =
                if not (inBoard (ar, ac)) then
                    rst
                else
                    loop (ar + x, ac + y) x y ((ar, ac) :: rst)

            loop (ar, ac) x y []

        points_dict.Values
        |> Array.ofSeq
        |> Seq.collect (fun idx_list ->
            idx_list
            |> getIdxPair
            |> List.collect (fun (l, r) -> getAntinodes getAntinodesInLine l r))
        |> Set.ofSeq
        |> Seq.length


type Day9(lines: string[]) =
    member this.Q1() =
        let transform (l: string) =
            l
            |> Seq.mapi (fun i c ->
                let n = char2Int c

                if i % 2 = 0 then
                    Array.init n (fun _ -> $"{i / 2}")
                else
                    Array.init n (fun _ -> "."))
            |> Seq.concat
            |> Seq.toArray

        let lines = String.Join("", lines) |> transform

        let rec shift i j =
            if i < j then
                match lines[i] = ".", lines[j] <> "." with
                | true, true ->
                    lines[i] <- lines[j]
                    lines[j] <- "."
                    shift (i + 1) (j - 1)
                | true, false -> shift i (j - 1)
                | false, true -> shift (i + 1) j
                | false, false -> shift (i + 1) (j - 1)

        shift 0 (lines.Length - 1)

        lines
        |> Array.mapi (fun i c -> if c <> "." then int64 i * (int64 c) else 0L)
        |> Array.sum

    member this.Q2() =
        let transform (l: string) =
            l
            |> Seq.mapi (fun i c ->
                let n = char2Int c

                if i % 2 = 0 then
                    Array.init n (fun idx -> (idx + 1, $"{i / 2}"))
                else
                    Array.init n (fun idx -> (n - idx, ".")))
            |> Seq.concat
            |> Seq.toArray

        let lines = String.Join("", lines) |> transform

        let rec shift i j =
            let ni, ci = lines[i]
            let nj, cj = lines[j]

            if i < j then
                match ci = ".", cj <> ".", ni >= nj with
                | true, true, true ->
                    for n in 0 .. nj - 1 do
                        lines[i + nj - n - 1] <- lines[j - n]
                        lines[j - n] <- (fst lines[j - n], ".")

                    shift 0 (j - nj)
                | true, true, false -> shift (i + ni) j
                | false, true, _ -> shift (i + 1) j
                | true, false, _ -> shift i (j - 1)
                | false, false, _ -> shift (i + 1) (j - 1)
            elif cj <> "." && j - nj > 0 then
                shift 0 (j - nj)

        shift 0 (lines.Length - 1)

        lines
        |> Array.mapi (fun i (_, c) -> if c <> "." then int64 i * (int64 c) else 0L)
        |> Array.sum

type Day10(lines: string[]) =
    let R = lines.Length
    let C = lines[0].Length
    let inBoard (i, j) : bool = 0 <= i && i < R && 0 <= j && j < C
    let lines = lines |> Array.map (fun s -> s.ToCharArray() |> Array.map char2Int)

    let rec move (i, j) =
        if lines[i][j] = 9 then
            [ (i, j) ]
        else
            [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
            |> List.map (fun (x, y) -> (i + x, j + y))
            |> List.filter (fun (x, y) -> inBoard (x, y) && lines[i][j] + 1 = lines[x][y])
            |> List.collect move

    let index_0 =
        [ for i in 0 .. R - 1 do
              for j in 0 .. C - 1 do
                  if lines[i][j] = 0 then
                      (i, j) ]

    member this.Q1() =
        index_0 |> List.sumBy (move >> Set.ofList >> Set.count)

    member this.Q2() =
        index_0 |> List.sumBy (move >> List.length)

type Day11(lines: string[]) =
    let nums = lines[0] |> split2Int |> List.ofArray |> List.map int64

    let transform n =

        if n = 0L then
            [ 1L ]
        elif $"{n}".Length % 2 = 0 then
            let div = pown 10L ($"{n}".Length / 2)
            [ n / div; n % div ]
        else
            [ n * 2024L ]

    let mem = Dictionary<int * int64, int64>()

    let rec loop i num =
        let key = (i, num)

        if mem.ContainsKey key then
            mem[key]
        else
            let rst =
                if i = 0 then
                    1L
                else
                    num |> transform |> List.sumBy (fun n -> loop (i - 1) n)

            mem[key] <- rst
            rst

    member this.Q1() = nums |> List.sumBy (loop 25)
    member this.Q2() = nums |> List.sumBy (loop 75)

type Day(lines: string[]) =
    member this.Q1() = 0
    member this.Q2() = 0

let start = DateTime.Now
let lines = File.ReadAllLines "test.in"
Day11(lines).Q1() |> (fun x -> printfn $"Q1={x}")
Day11(lines).Q2() |> (fun x -> printfn $"Q2={x}")
printfn $"Execution time: %A{(DateTime.Now - start).TotalSeconds} seconds"
