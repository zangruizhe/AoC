open System
open System.Collections.Generic
open System.Text.RegularExpressions

let getInput () =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/test.in")
    |> Seq.toArray
    |> fun x ->
        printfn $"read lines={x.Length}"
        x

let getIntFromStr (split: char) (s: string) =
    s.Trim().Split(split)
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map int64
    |> Seq.toArray

let getValueFromStr (convert) (split: char) (s: string) =
    s.Trim().Split(split)
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map convert
    |> Seq.toArray


let LOG x = printfn $"%A{x}"

let XLOG x =
    printfn $"%A{x}"
    x

module AoC2021 =
    module Day5 =
        let Q1 (input: string[]) =
            let points =
                input
                |> Array.collect (fun s ->
                    let reg = Regex.Matches(s, "(\d+)")

                    reg
                    |> Seq.map (fun m -> m.Groups[1].Value |> int)
                    |> Seq.toArray
                    |> fun nums ->
                        let x1 = nums[0]
                        let y1 = nums[1]
                        let x2 = nums[2]
                        let y2 = nums[3]

                        let points =
                            [| if x1 = x2 then
                                   yield! [| min y1 y2 .. max y1 y2 |] |> Array.map (fun y -> (x1, y))
                               if y1 = y2 then
                                   yield! [| min x1 x2 .. max x1 x2 |] |> Array.map (fun x -> (x, y1)) |]

                        points)

            let maps =
                points
                |> Array.fold
                    (fun map (x, y) -> Map.add (x, y) ((Map.tryFind (x, y) map |> Option.defaultValue 0) + 1) map)
                    Map.empty


            maps |> Map.values |> Seq.filter (fun v -> v >= 2) |> Seq.length

        let Q2 (input: string[]) =
            let points =
                input
                |> Array.collect (fun s ->
                    let reg = Regex.Matches(s, "(\d+)")

                    reg
                    |> Seq.map (fun m -> m.Groups[1].Value |> int)
                    |> Seq.toArray
                    |> fun nums ->
                        let x1, y1, x2, y2 =
                            if nums[0] <= nums[2] then
                                nums[0], nums[1], nums[2], nums[3]
                            else
                                nums[2], nums[3], nums[0], nums[1]

                        let points =
                            [| if x1 = x2 then
                                   yield! [| min y1 y2 .. max y1 y2 |] |> Array.map (fun y -> (x1, y))
                               elif y1 = y2 then
                                   yield! [| x1..x2 |] |> Array.map (fun x -> (x, y1))
                               elif abs (x1 - x2) = abs (y1 - y2) then
                                   let n = x2 - x1

                                   match y1 <= y2 with
                                   | true ->
                                       for i in 0..n do
                                           yield (x1 + i, y1 + i)
                                   | false ->
                                       for i in 0..n do
                                           yield (x1 + i, y1 - i) |]
                            |> Array.distinct

                        // printfn $"%A{nums} %A{points}"
                        points)

            let maps =
                points
                |> Array.fold
                    (fun map (x, y) -> Map.add (x, y) ((Map.tryFind (x, y) map |> Option.defaultValue 0) + 1) map)
                    Map.empty

            // printfn $"%A{Map.toArray maps}"

            maps |> Map.values |> Seq.filter (fun v -> v >= 2) |> Seq.length



module Day1 =
    let Q1 (input: string[]) =
        input
        |> Array.fold
            (fun s v ->
                let tmp = v.ToCharArray()
                let f = tmp |> Array.find (fun x -> '0' <= x && x <= '9')
                let b = tmp |> Array.findBack (fun x -> '0' <= x && x <= '9')
                s + int (f - '0') * 10 + int (b - '0'))
            0

    let Q2 (input: string[]) =
        let values =
            [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

        let getValue (str: string) (i: int) =
            let x = str[i]

            if '0' <= x && x <= '9' then
                int (x - '0') |> Some
            else
                values
                |> Array.tryFindIndex (fun v -> str.Substring(i).StartsWith(v))
                |> function
                    | Some i -> Some(i + 1)
                    | None -> None

        input
        |> Array.fold
            (fun s str ->
                let nums = [| 0 .. str.Length - 1 |] |> Array.choose (getValue str)
                s + nums[0] * 10 + nums[nums.Length - 1])
            0

    let Q2V2 (input: string[]) =

        let getValues (str: string) =
            let values =
                [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

            let getValue (str: string) =
                match Array.tryFindIndex (fun v -> v = str) values with
                | None -> int (str)
                | Some value -> value + 1

            let p = String.Join('|', values)
            let rst = Regex.Matches(str, "(?=(" + p + "|\d))")
            rst |> Seq.toArray |> Array.map (fun x -> getValue (x.Groups[1].Value))

        input
        |> Array.fold
            (fun s str ->
                let nums = getValues str
                s + nums[0] * 10 + nums[nums.Length - 1])
            0

module Day3 =
    let Q1 (input: string[]) =
        let R = input.Length
        let C = input[0].Length

        let mutable matrix = input |> Array.map (fun s -> s.ToCharArray())
        let mutable mark = Array.init R (fun i -> Array.create C false)
        let nums = [| 0..9 |] |> Array.map string |> Array.map char |> Set.ofArray

        printf $"%A{nums}"

        let is_num (v: char) = Set.contains v nums

        let is_symbol (v: char) = not (is_num v) && v <> '.'

        let rec dfs r c =
            if r < 0 || r >= R || c < 0 || c >= C || mark[r][c] = true then
                ()
            else if is_num (matrix[r][c]) then
                mark[r][c] <- true
                dfs r (c - 1)
                dfs r (c + 1)


        for r in 0 .. (R - 1) do
            for c in 0 .. (C - 1) do
                if is_symbol (matrix[r][c]) then
                    [| (0, -1); (-1, -1); (1, -1); (1, 0); (-1, 0); (0, 1); (-1, 1); (1, 1) |]
                    |> Array.iter (fun (i, j) -> dfs (r + i) (c + j))

        for r in 0 .. (R - 1) do
            for c in 0 .. (C - 1) do
                if mark[r][c] = false then
                    matrix[r][c] <- ' '

        printfn $"%A{matrix}"

        matrix
        |> Array.collect (fun s ->
            let str = String.Join("", s)
            let reg = Regex(@"(\d+)")
            reg.Matches(str) |> Seq.map (fun m -> int m.Groups[1].Value) |> Seq.toArray)
        |> Array.sum

    let Q2 (input: string[]) =
        let R = input.Length
        let C = input[0].Length

        let mutable matrix = input |> Array.map (fun s -> s.ToCharArray())
        let mutable mark = Array.init R (fun i -> Array.create C false)
        let mutable num_ids = Array.init R (fun i -> Array.create C -1)
        let nums = [| 0..9 |] |> Array.map string |> Array.map char |> Set.ofArray

        let is_num (v: char) = Set.contains v nums

        let is_symbol (v: char) = not (is_num v) && v <> '.'

        let rec dfs num_id r c =
            if
                r < 0
                || r >= R
                || c < 0
                || c >= C
                || mark[r][c] = true
                || not (is_num (matrix[r][c]))
            then
                false
            else
                mark[r][c] <- true
                num_ids[r][c] <- num_id
                dfs num_id r (c - 1) |> ignore
                dfs num_id r (c + 1) |> ignore
                true


        let mutable num_id = 0

        for r in 0 .. (R - 1) do
            for c in 0 .. (C - 1) do
                if is_symbol (matrix[r][c]) then
                    let rst =
                        [| (0, -1); (-1, -1); (1, -1); (1, 0); (-1, 0); (0, 1); (-1, 1); (1, 1) |]
                        |> Array.map (fun (i, j) -> dfs num_id (r + i) (c + j))
                        |> Array.exists id

                    match rst with
                    | true -> num_id <- num_id + 1
                    | false -> ()

        for r in 0 .. (R - 1) do
            for c in 0 .. (C - 1) do
                if mark[r][c] = false then
                    matrix[r][c] <- ' '

        printfn $"%A{matrix}"

        let num_with_ids =
            matrix
            |> Array.indexed
            |> Array.collect (fun (i, s) ->
                let str = String.Join("", s)
                let reg = Regex(@"(\d+)")

                reg.Matches(str)
                |> Seq.map (fun m -> num_ids[i][m.Groups[1].Index], int m.Groups[1].Value)
                |> Seq.toArray)

        num_with_ids
        |> Array.groupBy fst
        |> Array.filter (fun (_, arr) -> arr.Length >= 2)
        |> Array.sumBy (fun (_, arr) -> arr |> Array.map snd |> Array.reduce (*))

module Day4 =
    let getNums (str: string) =
        let reg = Regex(@"(\d+)")
        reg.Matches(str) |> Seq.map (fun m -> int m.Groups[1].Value) |> Seq.toArray

    let Q1 (input: string[]) =
        input
        |> Array.map (fun s ->
            let nums = (s.Split(':')[1]).Split('|')
            let win = getNums nums[0] |> Set.ofArray
            let have = getNums nums[1]

            have
            |> Array.filter (fun v -> Set.contains v win)
            |> fun tmp -> if tmp.Length > 0 then pown 2.0 (tmp.Length - 1) else 0)
        |> Array.sum

    let Q2 (input: string[]) =
        let numOfWin (s: string) =
            let nums = (s.Split(':')[1]).Split('|')
            let win = getNums nums[0] |> Set.ofArray
            let have = getNums nums[1]

            have |> Array.filter (fun v -> Set.contains v win) |> Array.length

        let play (card_num: int) (win: Dictionary<int, int>) =
            let win_num = numOfWin input[card_num]

            for i in 1..win_num do
                win[i + card_num] <- win[i + card_num] + win[card_num]

        let win = Dictionary<int, int>()

        for i in 0 .. (input.Length - 1) do
            win.Add(i, 1)

        input |> Seq.iteri (fun i _ -> play i win)
        win.Values |> Seq.sum

let AoCTest () =
    let rst = Regex.Matches("two29oneighteight1", "(?=(one|eight|two|\d))")

    rst
    |> Seq.toArray
    |> Array.iter (fun x -> printfn $"%A{x.Index} {x.Groups[1].Value} {x.Value}")

module Day5 =
    let getSeed (str: string) =
        str.Split(':')[1] |> getValueFromStr decimal ' '

    let getLocal (map: decimal[][][]) src =
        map
        |> Array.fold
            (fun s m ->
                match Array.tryFind (fun (arr: decimal[]) -> arr[1] <= s && s <= (arr[1] + arr[2] - 1M)) m with
                | None -> s
                | Some arr -> arr[0] + (s - arr[1]))
            src

    let getMaps (strs: string[]) =
        strs |> Array.map (getValueFromStr decimal ' ') |> Array.sortBy (fun x -> x[1])

    let Q1 (input: string[]) =
        let input = input |> Array.filter (fun s -> s.Length > 0)

        let seeds = getSeed input[0]

        let splits =
            input
            |> Array.indexed
            |> Array.filter (fun (i, x) -> x.EndsWith("map:"))
            |> Array.map fst

        let maps =
            splits
            |> Array.pairwise
            |> Array.map (fun (l, r) -> input[l + 1 .. r - 1] |> getMaps)

        let maps =
            let last = getMaps input[(Array.last splits) + 1 ..]
            Array.append maps [| last |]

        let locals = seeds |> Array.map (getLocal maps)
        locals |> Array.min

    let getLocals (map: decimal[][][]) (src: decimal[]) =
        let l = src[0]
        let r = src[1]

        let collect_pair (m: decimal[][]) (l, r) =
            let mutable l: decimal = l
            let mutable r: decimal = r
            let rst_range = ResizeArray()

            for range in m do
                if l <= r then
                    let L = range[1]
                    let R = range[1] + range[2] - 1M

                    if l < L then
                        rst_range.Add(l, min r L)
                        l <- L + 1M
                    else if l <= R then
                        rst_range.Add(range[0] + l - L, range[0] + (min r R) - L)
                        l <- (min r R) + 1M

            if l <= r then
                rst_range.Add(l, r)

            rst_range |> Seq.toArray

        map
        |> Array.fold (fun seed_ranges m -> seed_ranges |> Array.collect (collect_pair m)) [| (l, r) |]

    let Q2 (input: string[]) =
        let getSeedV2 (str: string) =
            str.Split(':')[1] |> getValueFromStr decimal ' ' |> Array.chunkBySize 2

        let input = input |> Array.filter (fun s -> s.Length > 0)

        let seeds = getSeedV2 input[0]

        let splits =
            input
            |> Array.indexed
            |> Array.filter (fun (i, x) -> x.EndsWith("map:"))
            |> Array.map fst

        let maps =
            splits
            |> Array.pairwise
            |> Array.map (fun (l, r) -> input[l + 1 .. r - 1] |> getMaps)

        let maps =
            let last = getMaps input[(Array.last splits) + 1 ..]
            Array.append maps [| last |]

        let locals =
            seeds
            |> Array.collect (fun seed ->
                let l = seed[0]
                let r = seed[0] + seed[1] - 1M
                getLocals maps [| l; r |])

        locals |> Array.minBy fst |> fst

printfn ">>>>>>>>>"
getInput () |> Day5.Q2 |> printfn "%A" // 52840
printfn "<<<<<<<<<"
