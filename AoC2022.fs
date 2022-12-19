module AoC2022
// https://adventofcode.com/2022

open System
open FsUnit
open Microsoft.FSharp.Core
open Xunit
open Xunit.Abstractions
open System.Collections.Generic

let XLOG x =
    printfn $"%A{x}"
    x

let LOG x = printfn $"%A{x}"

type Day1(output: ITestOutputHelper) =
    do new AocInput.Converter(output) |> Console.SetOut

    let GetCal (input: string) : int[] =
        input.Split("\n\n") |> Array.map (fun s -> s.Split("\n") |> Seq.sumBy int)

    let F1 (input: string) : int = input |> GetCal |> Array.max

    let F2 (input: string) : int =
        input |> GetCal |> Array.sortByDescending id |> Array.take 3 |> Array.sum

    [<Fact>]
    let ``Day 1`` () =
        "2022_D1.txt" |> AocInput.GetInputAsText |> F1 |> should equal 72511

        "2022_D1.txt" |> AocInput.GetInputAsText |> F2 |> should equal 212117


module Day2 =
    type GameResult =
        | Win
        | Lost
        | Draw

    type GameOP =
        | Rock
        | Paper
        | Scissor

    let ChoiceScore op =
        match op with
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

    let parse_other a =
        if a = 'A' then Rock
        elif a = 'B' then Paper
        else Scissor

    let parse_my_op a =
        if a = 'X' then Rock
        elif a = 'Y' then Paper
        else Scissor

    let F1 (input: string[]) : int =
        let GameScore (my: GameOP) (other: GameOP) : int =
            match my, other with
            | Rock, Scissor
            | Paper, Rock
            | Scissor, Paper -> 6

            | Rock, Paper
            | Paper, Scissor
            | Scissor, Rock -> 0

            | Rock, Rock
            | Paper, Paper
            | Scissor, Scissor -> 3

        input
        |> Array.map (fun l -> (parse_my_op l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameScore my other + ChoiceScore my)
        |> Array.sum

    let F2 (input: string[]) : int =
        let GameScore (rst: GameResult) (other: GameOP) : int =
            match rst, other with
            | Win, Scissor
            | Lost, Paper
            | Draw, Rock -> 1

            | Win, Rock
            | Lost, Scissor
            | Draw, Paper -> 2

            | Win, Paper
            | Lost, Rock
            | Draw, Scissor -> 3

        let parse_my_rst a =
            if a = 'X' then Lost
            elif a = 'Y' then Draw
            else Win

        let ResultScore rst =
            match rst with
            | Win -> 6
            | Lost -> 0
            | Draw -> 3

        input
        |> Array.map (fun l -> (parse_my_rst l[2]), (parse_other l[0]))
        |> Array.map (fun (my, other) -> GameScore my other + ResultScore my)
        |> Array.sum

    [<Fact>]
    let ``Day 2`` () =
        "2022_D2.txt" |> AocInput.GetInput |> F1 |> should equal 11603

        "2022_D2.txt" |> AocInput.GetInput |> F2 |> should equal 12725

module Day3 =
    let GetPrior (c: char) =
        if c >= 'a' then
            int c - int 'a' + 1
        else
            int c - int 'A' + 27

    let F1 (input: string[]) : int =
        input
        |> Seq.map (fun s -> s |> Seq.splitInto 2 |> Seq.map Set.ofSeq)
        |> Seq.collect Set.intersectMany
        |> Seq.sumBy GetPrior

    let F2 (input: string[]) : int =
        input
        |> Seq.map Set.ofSeq
        |> Seq.chunkBySize 3
        |> Seq.collect Set.intersectMany
        |> Seq.sumBy GetPrior

    [<Fact>]
    let ``Day 3`` () =
        "2022_D3.txt" |> AocInput.GetInput |> F1 |> should equal 7742

        "2022_D3.txt" |> AocInput.GetInput |> F2 |> should equal 2276

module Day4 =
    let Parse (input: string[]) =
        input |> Array.map (fun s -> s.Split([| '-'; ',' |]) |> Array.map int)

    let F1 (input: string[]) : int =
        input
        |> Parse
        |> Array.sumBy (fun num ->
            if (num[0] <= num[2] && num[1] >= num[3]) || (num[2] <= num[0] && num[3] >= num[1]) then
                1
            else
                0)

    let F2 (input: string[]) : int =
        input
        |> Parse
        |> Array.sumBy (fun num -> if num[2] <= num[1] && num[3] >= num[0] then 1 else 0)


    [<Fact>]
    let ``Day 4`` () =
        "2022_D4.txt" |> AocInput.GetInput |> F1 |> should equal 487

        "2022_D4.txt" |> AocInput.GetInput |> F2 |> should equal 849

module Day5 =
    let ParseStack (input: string[]) =
        let stack = Array.init 9 (fun _ -> List.empty)

        input
        |> Array.map (Seq.chunkBySize 4)
        |> Array.iter (fun s ->
            s
            |> Seq.iteri (fun i v ->
                if v[1] <> ' ' then
                    stack[i] <- stack[i] @ [ v[1] ]))

        stack

    let ParseCommand (input: string[]) =
        input
        |> Array.map (fun s -> s.Split(' ') |> (fun x -> int x[1], int x[3] - 1, int x[5] - 1))

    let ExeCommand (input: string[]) take_f =
        let split_i = input |> Array.findIndex (fun s -> s.Length = 0)

        let stack = input[.. split_i - 2] |> ParseStack

        let command = input[split_i + 1 ..] |> ParseCommand

        command
        |> Array.iter (fun (v, f, t) ->
            let take = stack[f] |> List.take v |> take_f
            stack[f] <- stack[f] |> List.skip v
            stack[t] <- take @ stack[t])

        stack
        |> Array.choose (fun s ->
            match s with
            | h :: _ -> Some h
            | [] -> None)
        |> String

    let F1 (input: string[]) : string = ExeCommand input List.rev

    let F2 (input: string[]) : string = ExeCommand input id

    [<Fact>]
    let ``Day 5`` () =
        "2022_D5.txt" |> AocInput.GetInput |> F1 |> should equal "FJSRQCFTN"

        "2022_D5.txt" |> AocInput.GetInput |> F2 |> should equal "CJVLJQPHS"

module Day6 =
    let GetIndex (input: string[]) (req: int) : int =
        let s = input.[0]
        let mem = Dictionary<char, int>()

        let rec Window l r =
            if r >= s.Length then
                r
            else if r - l = req then
                if mem.Count = req then
                    r
                else
                    mem[s[l]] <- mem[s[l]] - 1

                    if mem[s[l]] = 0 then
                        mem.Remove(s[l]) |> ignore

                    Window (l + 1) r
            else
                let c = s[r]

                match mem.ContainsKey(c) with
                | true -> mem[c] <- mem[c] + 1
                | false -> mem.Add(c, 1)

                Window l (r + 1)

        Window 0 0

    let F1 (input: string[]) : int = GetIndex input 4

    let F2 (input: string[]) : int =
        let i1 = GetIndex input 4

        i1 + GetIndex (input.[0][i1..] |> Array.singleton) 14

    [<Fact>]
    let ``Day 6`` () =
        "2022_D6.txt" |> AocInput.GetInput |> F1 |> should equal 1578

        "2022_D6.txt" |> AocInput.GetInput |> F2 |> should equal 2178

module Day7 =
    type FileSystem =
        | DIR of name: string
        | FILE of name: string * size: int

    type Token =
        | CD of string
        | LS
        | File of FileSystem

    type TokenStream = Token list

    let DirTree = Dictionary<string, FileSystem list>()

    let Mem = Dictionary<string, int>()

    let Tokenize (input: string[]) : TokenStream =
        input
        |> Seq.map (fun s ->
            let tmp = s.Split [| ' ' |]

            if tmp[0] = "$" then
                if tmp[1] = "ls" then LS else CD tmp[2]
            else if tmp[0] = "dir" then
                DIR tmp[1] |> File
            else
                (tmp[1], int tmp[0]) |> FILE |> File)
        |> Seq.toList

    let rec Parse (folder: string list) (tokens: TokenStream) =
        match tokens with
        | CD dir :: LS :: t -> Parse (dir :: folder) t
        | CD ".." :: t -> Parse folder.Tail t
        | File s :: t ->
            let path = String.Join("/", folder |> List.rev |> Seq.ofList)

            match DirTree.ContainsKey path with
            | true -> DirTree[path] <- s :: DirTree[path]
            | false -> DirTree.Add(path, [ s ])

            Parse folder t
        | _ -> ()


    let rec GetSize (dir: string) : int =
        match DirTree.ContainsKey(dir) with
        | true ->
            DirTree[dir]
            |> List.fold
                (fun pre f ->
                    match f with
                    | DIR d ->
                        let path = dir + "/" + d
                        let tmp = GetSize(path)
                        Mem.Add(path, tmp)
                        tmp
                    | FILE (_, size) -> size
                    |> (+) pre)
                0
        | false -> failwith $"can not find dir:{dir}"

    let F1 (input: string[]) : int =
        DirTree.Clear()
        Mem.Clear()

        input |> Tokenize |> Parse []

        GetSize "/" |> ignore

        Mem |> Seq.sumBy (fun i -> if i.Value <= 100000 then i.Value else 0)

    let F2 (input: string[]) : int =
        DirTree.Clear()
        Mem.Clear()
        input |> Tokenize |> Parse []

        Mem["/"] <- GetSize "/"

        let need_free = Mem["/"] - (70000000 - 30000000)

        Mem
        |> Seq.sortBy (fun i -> i.Value)
        |> Seq.find (fun i -> i.Value >= need_free)
        |> (fun i -> i.Value)

    [<Fact>]
    let ``Day 7`` () =
        "2022_D7.txt" |> AocInput.GetInput |> F1 |> should equal 1477771

        "2022_D7.txt" |> AocInput.GetInput |> F2 |> should equal 3579501

module Day8 =
    let CalculateMinOf4DirHigh (matrix: int[][]) : int[][] =
        let rst =
            Array.init matrix.Length (fun i -> Array.init matrix[0].Length (fun j -> matrix[i][j]))

        let CalRow i index =
            index
            |> Array.skip 1
            |> Array.fold
                (fun state a ->
                    rst[i][a] <- min (rst[i][a]) state
                    max (matrix[i][a]) state)
                (matrix[i][index[0]])
            |> ignore

        let CalCol i index =
            index
            |> Array.skip 1
            |> Array.fold
                (fun state a ->
                    rst[a][i] <- min (rst[a][i]) state
                    max (matrix[a][i]) state)
                (matrix[index[0]][i])
            |> ignore

        for i = 1 to (matrix.Length - 2) do
            let index = [| 0 .. matrix[0].Length - 1 |]
            index |> CalRow i
            index |> Array.rev |> CalRow i

        for i = 1 to (matrix[0].Length - 2) do
            let index = [| 0 .. matrix.Length - 1 |]
            index |> CalCol i
            index |> Array.rev |> CalCol i

        rst

    let ParseInputToMatrix (input: string[]) : int[][] =
        input
        |> Array.map (fun s -> s |> Seq.map (fun c -> int c - int '0') |> Seq.toArray)

    let F1 (input: string[]) : int =
        let matrix = ParseInputToMatrix input
        let cal_high = CalculateMinOf4DirHigh matrix

        [ for i = 1 to matrix.Length - 2 do
              for j = 1 to matrix[0].Length - 2 do
                  if matrix[i][j] > cal_high[i][j] then
                      yield 1 ]
        |> List.sum
        |> (+) (2 * matrix.Length + (matrix[0].Length - 2) * 2)

    type DIR =
        | Up
        | Down
        | Left
        | Right

    let rec WalkInMatrix (matrix: int[][]) start_p dir i j cur : int =
        if i < 0 || i >= matrix.Length || j < 0 || j >= matrix[0].Length then
            cur
        elif matrix[i][j] >= start_p then
            cur + 1
        else
            match dir with
            | Up -> WalkInMatrix matrix start_p dir (i - 1) j (cur + 1)
            | Down -> WalkInMatrix matrix start_p dir (i + 1) j (cur + 1)
            | Left -> WalkInMatrix matrix start_p dir i (j - 1) (cur + 1)
            | Right -> WalkInMatrix matrix start_p dir i (j + 1) (cur + 1)

    let F2 (input: string[]) : int =
        let matrix = ParseInputToMatrix input

        [ for i = 1 to matrix.Length - 2 do
              for j = 1 to matrix[0].Length - 2 do
                  yield
                      [ WalkInMatrix matrix (matrix[i][j]) Up (i - 1) j 0
                        WalkInMatrix matrix (matrix[i][j]) Down (i + 1) j 0
                        WalkInMatrix matrix (matrix[i][j]) Left i (j - 1) 0
                        WalkInMatrix matrix (matrix[i][j]) Right i (j + 1) 0 ]
                      |> List.reduce (*) ]
        |> List.max


    [<Fact>]
    let ``Day 8`` () =
        "2022_D8.txt" |> AocInput.GetInput |> F1 |> should equal 1814

        "2022_D8.txt" |> AocInput.GetInput |> F2 |> should equal 330786

module Day9 =
    let GetNextT (tr, tc) (hr, hc) : int * int =
        let r = abs (tr - hr)
        let c = abs (tc - hc)

        if r <= 1 && c <= 1 then
            (tr, tc)
        else
            tr + compare hr tr, tc + compare hc tc

    let GetHeadPos (input: string[]) =
        input
        |> Array.mapFold
            (fun (hr, hc) s ->
                let tmp = s.Split ' '

                let op_r, op_c =
                    match tmp[0] with
                    | "L" -> (0, -1)
                    | "R" -> (0, 1)
                    | "U" -> (-1, 0)
                    | "D" -> (1, 0)
                    | _ -> failwith $"wrong op:{s}"

                [| 1 .. int tmp[1] |]
                |> Array.mapFold
                    (fun (r, c) _ ->
                        let h = (r + op_r, c + op_c)
                        h, h)
                    (hr, hc))
            (0, 0)
        |> fst
        |> Array.concat

    let GetTailPos n head_pos =
        head_pos
        |> Array.fold
            (fun (rst, tail) head ->
                let next = tail |> Array.scan (fun pre t -> GetNextT t pre) head |> Array.tail
                (Array.last next) :: rst, next)
            ([], Array.replicate n (0, 0))
        |> fst

    let F1 (input: string[]) : int =
        input |> GetHeadPos |> GetTailPos 1 |> Seq.distinct |> Seq.length

    let F2 (input: string[]) : int =
        input |> GetHeadPos |> GetTailPos 9 |> Seq.distinct |> Seq.length

    [<Fact>]
    let ``Day 9`` () =
        "2022_D9.txt" |> AocInput.GetInput |> F1 |> should equal 6067
        "2022_D9.txt" |> AocInput.GetInput |> F2 |> should equal 2471

module Day10 =
    let ParseInput (input: string[]) : int[] =
        input
        |> Array.fold
            (fun (v, rst) s ->
                let tmp = s.Split ' '

                if tmp.Length = 2 then
                    v + int tmp[1], [ v; v ] @ rst
                else
                    v, v :: rst

                )
            (1, [])
        |> snd
        |> List.rev
        |> List.toArray

    let F1 (input: string[]) : int =
        let mem = ParseInput input |> Array.append [| 0 |]
        [| 20..40..220 |] |> Array.map (fun i -> i * mem[i]) |> Array.sum

    let F2 (input: string[]) =
        input
        |> ParseInput
        |> Array.mapi (fun i v ->
            let i = i % 40
            if i >= v - 1 && i <= v + 1 then "#" else ".")
        |> Array.splitInto 6
        |> Array.iter (fun v -> printfn "%A" (String.concat "" v))
    // Result is RBPARAGF

    [<Fact>]
    let ``Day 10`` () =
        "2022_D10.txt" |> AocInput.GetInput |> F1 |> should equal 12740

module Day11 =
    let Operator (op: string) (op_v: string) div_v path1 path2 relief input : int * uint64 =
        let op_value = if op_v = "old" then input else uint64 op_v

        match op with
        | "+" -> input + op_value
        | "-" -> input - op_value
        | "*" -> input * op_value
        | "/" -> input / op_value
        | _ -> failwith $"can not parse op={op}"
        |> (fun x -> let t = relief x in if t % div_v = 0UL then path1, t else path2, t)

    let ParseInput (input: string) =
        input.Split "\n\n"
        |> Array.map (fun s ->
            let tmp = s.Split '\n'

            let item =
                tmp[1].Split(':').[1]
                    .Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map uint64
                |> Array.toList

            let op, op_v = let x = tmp[ 2 ].Split(' ') in x[x.Length - 2], x[x.Length - 1]
            let div_v = tmp[ 3 ].Split(' ') |> Array.last |> uint64
            let path1 = tmp[ 4 ].Split(' ') |> Array.last |> int
            let path2 = tmp[ 5 ].Split(' ') |> Array.last |> int
            (item, div_v), Operator op op_v div_v path1 path2)

    let Solve start_v (monkey_op: ((uint64 -> uint64) -> uint64 -> int * uint64)[]) relief loop_len =
        [| 1..loop_len |]
        |> Array.fold
            (fun (pre_game: uint64 list[], rst: int[]) _ ->
                pre_game
                |> Array.iteri (fun i iterm ->
                    rst[i] <- rst[i] + iterm.Length

                    iterm
                    |> List.iteri (fun _ v ->
                        let next, next_v = monkey_op.[i] relief v
                        pre_game[next] <- pre_game[next] @ [ next_v ])

                    pre_game[i] <- [])

                pre_game, rst)
            (start_v, Array.zeroCreate start_v.Length)
        |> snd
        |> Array.sortDescending
        |> (fun x -> uint64 x[0] * uint64 x[1])

    let F1 (input: string) =
        let monkey_val = ParseInput input
        let start_v = monkey_val |> Array.map (fst >> fst)
        let monkey_op = monkey_val |> Array.map snd
        let relief = (fun x -> x / 3UL)

        Solve start_v monkey_op relief 20

    let F2 (input: string) =
        let monkey_val = ParseInput input
        let start_v = monkey_val |> Array.map (fst >> fst)
        let monkey_op = monkey_val |> Array.map snd
        let mod_v = monkey_val |> Array.map (fst >> snd) |> Array.reduce (*)
        let relief = (fun x -> x % (uint64 mod_v))
        Solve start_v monkey_op relief 10_000

    [<Fact>]
    let ``Day 11`` () =
        "2022_D11.txt" |> AocInput.GetInputAsText |> F1 |> should equal 67830UL
        "2022_D11.txt" |> AocInput.GetInputAsText |> F2 |> should equal 15305381442UL

module Day12 =
    let Solve target (input: string[]) =
        let graph =
            Array.init input.Length (fun i -> Array.init input[0].Length (fun j -> input[i].[j]))

        let visited = Array2D.init input.Length input[0].Length (fun _ _ -> false)

        let rec bfs cur next : int =
            let get_target =
                next |> List.exists (fun (i, j) -> graph[i].[j] = target || graph[i].[j] = 'S')

            match get_target with
            | true -> cur
            | false ->
                next
                |> List.collect (fun (i, j) ->
                    [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
                    |> List.choose (fun (r, c) ->
                        let m = r + i
                        let n = c + j

                        if m < 0 || m >= graph.Length || n < 0 || n >= graph[0].Length then
                            None
                        else
                            let v = if graph[m].[n] = 'S' then 'a' else graph[m].[n]

                            if ((int v) >= (int graph[i].[j] - 1) && visited[m, n] = false) then
                                visited[m, n] <- true
                                Some(m, n)
                            else
                                None))
                |> fun next -> if next.IsEmpty then Int32.MaxValue else bfs (cur + 1) next

        let x, y =
            [ for i = 0 to graph.Length - 1 do
                  for j = 0 to graph[0].Length - 1 do
                      if graph[i][j] = 'E' then
                          graph[i][j] <- 'z'
                          yield (i, j) ]
            |> List.head

        visited[x, y] <- true
        bfs 0 [ (x, y) ]

    let F1 (input: string[]) = Solve 'S' input
    let F2 (input: string[]) = Solve 'a' input

    [<Fact>]
    let ``Day 12`` () =
        "2022_D12.txt" |> AocInput.GetInput |> F1 |> should equal 517
        "2022_D12.txt" |> AocInput.GetInput |> F2 |> should equal 512

module Day13 =
    type Signal =
        | NUM of int
        | LIST of Signal list

    let rec Parse (nums: string) =
        let rec ParseWithI i cur_num (cur: Signal list) =
            let GetCur () =
                (if cur_num >= 0 then ((NUM cur_num) :: cur) else cur)

            if i = nums.Length then
                [], i
            else
                match nums[i] with
                | '[' ->
                    let pre = GetCur() |> List.rev
                    let in_bracket, next_i = ParseWithI (i + 1) -1 []

                    ParseWithI next_i -1 []
                    |> (fun (next, i) -> pre @ [ in_bracket |> LIST ] @ next, i)

                | ']' -> GetCur() |> List.rev, (i + 1)
                | ',' -> ParseWithI (i + 1) -1 (GetCur())
                | c -> ParseWithI (i + 1) ((max cur_num 0) * 10 + AocInput.Char2Int c) cur

        ParseWithI 0 -1 [] |> fst |> List.head

    let rec Compare (a: Signal) (b: Signal) =
        match a, b with
        | LIST [], LIST [] -> None
        | LIST [], LIST _ -> Some true
        | LIST _, LIST [] -> Some false
        | LIST (l :: lt), LIST (r :: rt) ->
            match Compare l r with
            | Some v -> Some v
            | None -> Compare (LIST lt) (LIST rt)
        | LIST _, NUM _ -> Compare a (LIST [ b ])
        | NUM _, LIST _ -> Compare (LIST [ a ]) b
        | NUM i, NUM j ->
            if i < j then Some true
            elif i > j then Some false
            else None


    let F1 (input: string) =
        let Check (line: string) =
            line.Split "\n"
            |> Array.map Parse
            |> (fun x -> Compare x[0] x[1])
            |> Option.defaultValue true

        input.Split "\n\n"
        |> Array.mapi (fun i s ->
            match Check s with
            | true -> i + 1
            | false -> 0)
        |> Array.sum

    let F2 (input: string) =
        input.Split "\n\n"
        |> Array.collect (fun s -> s.Split "\n")
        |> Array.append [| "[[2]]"; "[[6]]" |]
        |> Array.sortWith (fun l r ->
            match Compare (Parse l) (Parse r) with
            | Some true -> -1
            | Some false -> 1
            | None -> 0)
        |> Array.indexed
        |> Array.filter (fun (_, s) -> s = "[[2]]" || s = "[[6]]")
        |> Array.fold (fun pre (i, _) -> pre * (i + 1)) 1


    [<Fact>]
    let ``Day 13`` () =
        "2022_D13.txt" |> AocInput.GetInputAsText |> F1 |> should equal 6478
        "2022_D13.txt" |> AocInput.GetInputAsText |> F2 |> should equal 21922

module Day14 =
    let GetWholeRock (input: string[]) : (int * int)[] =
        input
        |> Array.collect (fun s ->
            s.Split " -> "
            |> Array.map (fun s -> let p = s.Split ',' in int p[0], int p[1])
            |> Array.pairwise
            |> Array.collect (fun ((x, y), (x1, y1)) ->
                if x = x1 then
                    [| for i in [ (min y y1) .. (max y y1) ] -> (x, i) |]
                else
                    [| for i in [ (min x x1) .. (max x x1) ] -> (i, y) |]))

    let rec DropSand (matrix: char[][]) (x, y) : int * int =
        if x = 0 || x = matrix[0].Length - 1 || y >= matrix.Length - 1 then
            (x, y)
        else if matrix[y + 1][x] = '.' then
            DropSand matrix (x, y + 1)
        elif matrix[y + 1][x - 1] = '.' then
            DropSand matrix (x - 1, y + 1)
        elif matrix[y + 1][x + 1] = '.' then
            DropSand matrix (x + 1, y + 1)
        else
            (x, y)

    let rec PlayGame (matrix: char[][]) x i =
        let rest_x, rest_y = DropSand matrix (x, 0)

        if
            rest_x = matrix[0].Length - 1
            || rest_x = 0
            || rest_y >= matrix.Length - 1
            || rest_y = 0
        then
            i
        else
            matrix[rest_y][rest_x] <- '0'
            PlayGame matrix x (i + 1)

    let F1 (input: string[]) =
        let rocks = input |> GetWholeRock

        let min_x, max_x, max_y =
            rocks
            |> Array.fold
                (fun (min_x, max_x, max_y) (x, y) -> (min min_x x), (max max_x x), (max max_y y))
                (500, 500, 0)

        let matrix =
            Array.init (max_y + 1) (fun _ -> Array.init (max_x - min_x + 1) (fun _ -> '.'))

        rocks |> Array.iter (fun (x, y) -> matrix[y][x - min_x] <- '#')

        PlayGame matrix (500 - min_x) 0

    let F2 (input: string[]) =
        let rocks = input |> GetWholeRock

        let max_y = rocks |> Array.fold (fun max_y (_, y) -> (max max_y y)) 0 |> (+) 2

        let min_x = 500 - max_y
        let max_x = 500 + max_y

        let matrix =
            Array.init (max_y + 1) (fun _ -> Array.init (max_x - min_x + 1) (fun _ -> '.'))

        rocks |> Array.iter (fun (x, y) -> matrix[y][x - min_x] <- '#')

        for i in 0 .. matrix.[0].Length - 1 do
            matrix[matrix.Length - 1][i] <- '#'

        PlayGame matrix (500 - min_x) 1


    [<Fact>]
    let ``Day 14`` () =
        "2022_D14.txt" |> AocInput.GetInput |> F1 |> should equal 768
        "2022_D14.txt" |> AocInput.GetInput |> F2 |> should equal 26686

module Day15 =
    let GetSensorAndBeacon (str: string) =
        str.Split([| ' '; ':'; '='; ',' |])
        |> (fun x -> (int x[3], int x[6]), (int x[13], int x[16]))

    let GetLineEmptyPos sen_ben target =
        sen_ben
        |> Array.choose (fun ((x, y), (x1, y1)) ->
            let len = abs (x1 - x) + abs (y1 - y)

            if (abs (target - y) <= len) then
                let diff = len - abs (target - y)
                Some((x - diff), (x + diff))
            else
                None)
        |> Array.sortBy fst
        |> Array.fold
            (fun rst (x1, x2) ->
                match rst with
                | [] -> [ (x1, x2) ]
                | (pre1, pre2) :: t when pre2 >= x1 -> (pre1, (max x2 pre2)) :: t
                | _ -> (x1, x2) :: rst)
            []
        |> List.rev


    let F1 (input: string[]) =
        let target = 2000000
        let sen_ben = input |> Array.map GetSensorAndBeacon

        let use_pos =
            sen_ben
            |> Array.collect (fun ((x, y), (x1, y1)) ->
                [| if (y = target) then
                       yield (x, y)
                   if (y1 = target) then
                       yield (x1, y1) |])
            |> Array.distinct
            |> Array.length

        GetLineEmptyPos sen_ben target
        |> List.map (fun (x1, x2) -> x2 - x1 + 1)
        |> List.sum
        |> (fun x -> x - use_pos)


    let F2 (input: string[]) =
        let sen_ben = input |> Array.map (fun x -> GetSensorAndBeacon x)
        let limit = 4000000

        [ limit .. -1 .. 0 ]
        |> List.pick (fun target ->
            let lines = GetLineEmptyPos sen_ben target |> Array.ofList

            if lines.Length = 1 && fst lines[0] > 0 || snd lines[0] < limit then
                Some(target, lines)
            elif lines.Length > 1 then
                Some(target, lines)
            else
                None)
        |> (fun (y, x_list) ->
            if x_list.Length = 1 then
                if fst x_list[0] = 0 then snd x_list[0]
                elif snd x_list[0] = limit then 0
                else failwith "can not be here"
            else
                snd x_list[0] + 1
            |> fun x -> (uint64 y) + (uint64 limit) * (uint64 x))

    [<Fact>]
    let ``Day 15`` () =
        "2022_D15.txt" |> AocInput.GetInput |> F1 |> should equal 5394423
        "2022_D15.txt" |> AocInput.GetInput |> F2 |> should equal 11840879211051UL

module Day16 =
    let ParseInput (input: string) =
        input.Split([| ' '; '='; ';'; ',' |])
        |> (fun x ->
            let connect =
                x
                |> Array.filter (fun s -> s.Length = 2 && int s[0] >= int 'A' && int s[0] <= int 'Z')
                |> Array.tail

            (x[1], int x[5], connect))

    let Solution (input: string[]) start_time player =
        let keys, rate, connects =
            input
            |> Array.fold
                (fun (key, rate, connect) s ->
                    let t_k, t_r, t_c = ParseInput s
                    t_k :: key, t_r :: rate, t_c :: connect)
                ([], [], [])
            |> fun (key, rat, cnt) -> key |> List.toArray, rat |> List.toArray, cnt |> List.toArray


        let GetIndex (key: string) =
            keys |> Array.findIndex (fun x -> x = key)

        let N = keys.Length
        let graph = Array.init N (fun _ -> Array.create N 0)

        for i in 0 .. N - 1 do
            connects[i] |> Array.iter (fun j -> graph[GetIndex keys[i]][GetIndex j] <- 1)

        let Good = rate |> Array.indexed |> Array.filter (fun (i, v) -> v > 0)
        Good |> Array.iter (fun (i, v) -> printfn $"{keys[i]}={v}")

        let GoodIndex =
            let cache = Dictionary<int, int>()
            Good |> Array.iteri (fun j (i, v) -> cache[i] <- j)
            cache

        let GetGoodIndex x = GoodIndex[x]

        let rate_N = Good.Length
        let rate_S = (1 <<< rate_N) - 1

        let full_time = 30
        let mem = Array.create (N * (full_time + 1) * (rate_S + 1) * 2) -1

        let rec dfs full_time (start: int) cur_t rate_s player =
            let key =
                start * (full_time + 1) * (rate_S + 1) * 2
                + cur_t * (rate_S + 1) * 2
                + rate_s * 2
                + player

            if cur_t = 0 then
                if player = 0 then
                    0
                else
                    (dfs full_time (GetIndex "AA") full_time rate_s 0)
            elif mem[key] <> -1 then
                mem[key]
            else
                let just_open =
                    if rate[start] > 0 && (rate_s &&& (1 <<< GetGoodIndex(start))) > 0 then
                        (cur_t - 1) * rate[start]
                    else
                        0

                graph[start]
                |> Array.mapi (fun i v ->
                    [ if v > 0 then
                          yield dfs full_time i (cur_t - 1) rate_s player

                          if cur_t > 2 && just_open > 0 then
                              let next_rate_s = rate_s ^^^ (1 <<< GetGoodIndex(start))
                              yield just_open + dfs full_time i (cur_t - 2) next_rate_s player
                      else
                          yield just_open ]
                    |> Seq.max)
                |> Array.max
                |> fun x ->
                    mem[key] <- x
                    x

        dfs start_time (GetIndex "AA") start_time rate_S player

    let F1 (input: string[]) = Solution input 30 0
    let F2 (input: string[]) = Solution input 26 1

    [<Fact>]
    let ``Day 16`` () =
        "2022_D16.txt" |> AocInput.GetInput |> F1 |> should equal 1720
        "2022_D16.txt" |> AocInput.GetInput |> F2 |> should equal 2582

module Day17 =
    type Brick = (int64 * int64)[]

    let Solution (input: string[]) level =
        let jets =
            input
            |> Array.collect (fun s -> s.ToCharArray())
            |> Array.map (fun c ->
                if c = '<' then (-1L, 0L)
                elif c = '>' then (1L, 0L)
                else failwith "can not be here")

        let graph = HashSet<int64 * int64>()

        let MoveByJet (jx, jy) (brick: Brick) =
            brick
            |> Array.choose (fun (x, y) ->
                let tx, ty = (x + jx, y + jy)

                if tx >= 0 && tx < 7 && graph.Contains(tx, ty) = false then
                    Some(tx, ty)
                else
                    None)
            |> fun rst -> if rst.Length = brick.Length then rst else brick

        let MoveDown (brick: Brick) : bool * Brick =
            brick
            |> Array.choose (fun (x, y) ->
                let tx, ty = (x, y - 1L)

                if ty > 0 && graph.Contains(tx, ty) = false then
                    Some(tx, ty)
                else
                    None)
            |> fun rst ->
                if rst.Length = brick.Length then
                    true, rst
                else
                    false, brick

        let GetNextBrick n bottom : Brick =
            match n with
            | 0 -> [| (2, 0); (3, 0); (4, 0); (5, 0) |]
            | 1 -> [| (3, 2); (2, 1); (3, 1); (4, 1); (3, 0) |]
            | 2 -> [| (4, 2); (4, 1); (2, 0); (3, 0); (4, 0) |]
            | 3 -> [| (2, 3); (2, 2); (2, 1); (2, 0) |]
            | 4 -> [| (2, 1); (3, 1); (2, 0); (3, 0) |]
            | _ -> failwith $"unknown brick num:{n}"
            |> Array.map (fun (x, y) -> int64 x, (int64 y + bottom + 4L))

        let GetNextJet n = jets[n]

        let mutable rep_jet = 0

        let mutable pre_jet_high = 0UL
        let mutable rep_brick_high = 0UL

        let mutable pre_brick_num = 0UL
        let mutable rep_brick_num = 0UL

        let mutable skip_num = 0UL
        let target_jet = 2

        let rec PlayGame brick_num brick jet_num high : uint64 =
            if brick_num >= level then
                let tmp = skip_num * rep_brick_high
                graph |> Seq.maxBy snd |> snd |> uint64 |> (+) tmp
            else
                let jet = GetNextJet(jet_num % jets.Length)
                let next_jet = (jet_num + 1) % jets.Length
                let brick = MoveByJet jet brick

                let good, brick = MoveDown brick

                if good then
                    PlayGame brick_num brick next_jet high
                else
                    brick |> Array.iter (fun (x, y) -> graph.Add(x, y) |> ignore)
                    let next_high = max high (uint64 (snd brick[0]))
                    let brick = GetNextBrick (int ((brick_num + 1UL) % 5UL)) (int64 next_high)

                    if jet_num = target_jet && rep_jet < 5 then
                        // get stable repeat patten
                        rep_jet <- rep_jet + 1
                        rep_brick_num <- brick_num - pre_brick_num
                        rep_brick_high <- next_high - pre_jet_high
                        pre_jet_high <- next_high
                        pre_brick_num <- brick_num

                    if
                        jet_num = target_jet
                        && rep_jet >= 5
                        && (brick_num + rep_brick_num) < (level - rep_brick_num)
                    then
                        let tmp = (level - brick_num) / rep_brick_num - 1UL
                        skip_num <- tmp
                        PlayGame (brick_num + rep_brick_num * tmp + 1UL) brick next_jet next_high
                    else
                        PlayGame (brick_num + 1UL) brick next_jet next_high

        PlayGame 0UL (GetNextBrick 0 0) 0 0UL

    let F1 (input: string[]) = Solution input 2022UL
    let F2 (input: string[]) = Solution input 1000000000000UL

    [<Fact>]
    let ``Day 17`` () =
        "2022_D17.txt" |> AocInput.GetInput |> F1 |> should equal 3219UL
        "2022_D17.txt" |> AocInput.GetInput |> F2 |> should equal 1582758620701UL

module Day18 =
    type Point = int * int * int
    let diff = [| (1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1) |]

    let Solution (input: string[]) search_area =
        let points =
            input
            |> Array.map (fun s -> s.Split ',' |> (fun p -> (int p[0], int p[1], int p[2])))
            |> Set.ofArray

        let Out = HashSet<Point>()
        let In = HashSet<Point>()

        let rec bfs (cur_points: Point[]) (seen: HashSet<Point>) =
            if cur_points |> Array.exists In.Contains then
                false
            elif cur_points |> Array.exists Out.Contains then
                true
            elif cur_points.Length = 0 then
                seen |> Seq.map In.Add |> Seq.toArray |> ignore
                false
            elif seen.Count > search_area then
                seen |> Seq.map Out.Add |> Seq.toArray |> ignore
                true
            else
                let choose =
                    cur_points |> Array.filter (fun p -> not (seen.Contains p || points.Contains p))

                choose |> Array.map seen.Add |> ignore

                let next =
                    choose
                    |> Array.collect (fun (x, y, z) -> diff |> Array.map (fun (dx, dy, dz) -> x + dx, y + dy, z + dz))
                    |> Array.distinct

                bfs next seen

        points
        |> Seq.toArray
        |> Array.map (fun (x, y, z) ->
            diff
            |> Array.filter (fun (dx, dy, dz) ->
                let seen = HashSet<Point>()
                bfs [| x + dx, y + dy, z + dz |] seen)
            |> Array.length)
        |> Array.sum


    let F1 (input: string[]) = Solution input 0
    let F2 (input: string[]) = Solution input 5000


    [<Fact>]
    let ``Day 18`` () =
        "2022_D18.txt" |> AocInput.GetInput |> F1 |> should equal 3494
        "2022_D18.txt" |> AocInput.GetInput |> F2 |> should equal 2062

module Day19 =
    let F1 (input: string[]) = 0
    let F2 (input: string[]) = 0


    [<Fact>]
    let ``Day 19`` () =
        "2022_D19.txt" |> AocInput.GetInput |> F1 |> should equal 3494
        "2022_D19.txt" |> AocInput.GetInput |> F2 |> should equal 2062
