module Aoc2019

open Xunit
open FsUnit

module Helper =
    let rec distribute e l =
        match l with
        | [] -> [ [ e ] ]
        | x :: xs' as xs ->
            (e :: xs)
            :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute l =
        match l with
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

module Day1 =
    let F1 input =
        input
        |> List.sumBy (fun x -> (x / 3) - 2)
        |> printfn "%A"


    let F2 input =
        let rec get_fuel mass acc =
            let need = ((mass / 3) - 2)

            match need with
            | x when x <= 0 -> acc
            | _ -> get_fuel need (acc + need)

        input
        |> List.sumBy (fun x -> get_fuel x 0)
        |> printfn "%A"

module Day2 =
    let cpu_operation op a b r (result: int []) =
        result.[r] <- op result.[a] result.[b]
        ()

    let F1 (nonu: int) (verb: int) (input_list: int []) =
        let result = Array.copy input_list

        result.[1] <- nonu
        result.[2] <- verb


        let exp = result |> Array.chunkBySize 4

        let last_index =
            exp |> Array.findIndex (fun i -> i.[0] = 99)

        exp.[0..last_index]
        |> Array.iter (fun item ->
            let op = item.[0]

            match op with
            | 1 -> cpu_operation (+) item.[1] item.[2] item.[3] result
            | 2 -> cpu_operation (*) item.[1] item.[2] item.[3] result
            | _ -> ())

        result.[0]

    let F1v2 (nonu: int) (verb: int) (input_list: int []) =
        input_list.[1] <- nonu
        input_list.[2] <- verb

        let rec parse (input_list: int []) (result: int []) : int [] =

            let wrong () =
                printfn "error op"
                result

            if input_list.Length > 4 then
                let express = input_list |> Array.take 4

                match (express.[0], express.[1..]) with
                | 1, [| a; b; r |] ->
                    cpu_operation (+) a b r result
                    parse (Array.skip 4 input_list) result
                | 2, [| a; b; r |] ->
                    cpu_operation (*) a b r result
                    parse (Array.skip 4 input_list) result
                | 99, _ -> result
                | _ -> wrong ()
            else
                result

        let result = parse input_list input_list

        printfn $"Day F1v2 result: %A{result.[0]} = 9581917"
        input_list.[0]

    let F2 target (input: string) =

        let rec check (i: int) (j: int) (input: string) =
            if i < 100 && j < 100 then
                let r =
                    F1 i j (input.Split(",") |> Array.map int)

                match r with
                | x when x = target -> Some(i * 100 + j)
                | x when x < target ->
                    match (check (i + 1) j input) with
                    | Some v -> Some v
                    | None ->
                        match (check i (j + 1) input) with
                        | Some v -> Some v
                        | None -> None
                | _ -> None
            else
                None

        check 0 0 input

[<Fact>]
let ``Day2 `` () =
    Day2.F2 19690720 AocInput.Aoc2019.D2
    |> should equal (Some 2505)

module Day3 =
    type Point = { X: int; Y: int }

    type Dir =
        | U
        | D
        | L
        | R

    type Move = { Direction: Dir; Distance: int }

    let ManhattanDistance (p1: Point) (p2: Point) =
        [| (p1.X - p2.X); (p1.Y - p2.Y) |]
        |> Array.sumBy abs

    let GetLinePoints (pre_line: Point []) (move: Move) : Point [] =
        let start_point = Array.last pre_line
        let step = [| 1 .. move.Distance |]

        let tmp_line =
            match move.Direction with
            | U ->
                step
                |> Array.map (fun s -> { start_point with Y = start_point.Y + s })
            | D ->
                step
                |> Array.map (fun s -> { start_point with Y = start_point.Y - s })
            | L ->
                step
                |> Array.map (fun s -> { start_point with X = start_point.X - s })
            | R ->
                step
                |> Array.map (fun s -> { start_point with X = start_point.X + s })

        Array.append pre_line tmp_line

    let GetLine (move: string) =
        let paseAction (action: string) =
            let dir =
                match action.[0] with
                | 'U' -> U
                | 'D' -> D
                | 'R' -> R
                | 'L' -> L
                | x -> failwith (sprintf "error op %c" x)

            let move = action.[1..] |> int

            { Direction = dir; Distance = move }

        let start_point = { X = 0; Y = 0 }

        (move.Split ",")
        |> Array.map paseAction
        |> Array.fold
            (fun pre_list m ->
                let r = GetLinePoints pre_list m
                r)
            [| start_point |]

    let F1 (moves: string []) : int =

        let line1 = (GetLine moves.[0]).[1..]
        let line2 = (GetLine moves.[1]).[1..]

        let crossPoint =
            Set.toArray (Set.intersect (Set.ofArray line1) (Set.ofArray line2))

        let start_point = { X = 0; Y = 0 }

        let result =
            crossPoint
            |> Array.map (ManhattanDistance start_point)
            |> Array.min

        result

    let F2 (moves: string []) : int =
        let line1 = (GetLine moves.[0]).[1..]
        let line2 = (GetLine moves.[1]).[1..]

        let crossPoint =
            Set.toArray (Set.intersect (Set.ofArray line1) (Set.ofArray line2))

        let r =
            crossPoint
            |> Seq.map (fun p ->
                let d1 =
                    1 + Array.findIndex (fun i -> i = p) line1

                let d2 =
                    1 + Array.findIndex (fun i -> i = p) line2

                d1 + d2)
            |> Seq.min

        r

module Day4 =
    let oneAdjSameQ2 (x: char []) : bool =
        let r =
            x
            |> Array.pairwise
            |> Array.map (fun (i, j) -> i = j)

        let mutable result = false

        for i in [| 0 .. (r.Length - 1) |] do
            if r.[i] = true then
                if i = 0 then
                    if r.[i + 1] = false then result <- true
                elif i = (r.Length - 1) then
                    if r.[i - 1] = false then result <- true
                elif r.[i - 1] = false then
                    if r.[i + 1] = false then result <- true

        result

    let oneAdjSame (x: char []) : bool =
        let r =
            x
            |> Array.pairwise
            |> Array.exists (fun (i, j) -> i = j)

        r = true

    let decrease (x: char []) : bool =
        let r =
            x
            |> Array.pairwise
            |> Array.forall (fun (i, j) -> i <= j)

        r = true

    let F1 (s: int) (e: int) : int =
        let choose = [| s..e |]

        let r =
            choose
            |> Array.map (fun x -> (string x).ToCharArray())
            |> Array.filter (fun x -> (x.Length = 6) && (oneAdjSame x) && (decrease x))

        r.Length

    let F2 (s: int) (e: int) : int =
        let choose = [| s..e |]

        let r =
            choose
            |> Array.map (fun x -> (string x).ToCharArray())
            |> Array.filter (fun x -> (x.Length = 6) && (oneAdjSameQ2 x) && (decrease x))

        r.Length

[<Fact>]
let ``Day 4`` () =
    Day4.F1 193651 649729 |> should equal 1605
    Day4.F2 193651 649729 |> should equal 1102

module Day5 =
    type Expression =
        | Immediate of int
        | Add of Expression * Expression
        | Multiply of Expression * Expression
        | IfTrue of Expression * Expression * int
        | IfFalse of Expression * Expression * int
        | LessThan of Expression * Expression
        | Equal of Expression * Expression
        | Position of int

    let rec Evaluate (env: array<int>) exp =
        match exp with
        | Immediate n -> n
        | Add (x, y) -> Evaluate env x + Evaluate env y
        | Multiply (x, y) -> Evaluate env x * Evaluate env y
        | IfTrue (x, y, i) -> if Evaluate env x <> 0 then Evaluate env y else i + 3
        | IfFalse (x, y, i) -> if Evaluate env x = 0 then Evaluate env y else i + 3
        | LessThan (x, y) -> if Evaluate env x < Evaluate env y then 1 else 0
        | Equal (x, y) -> if Evaluate env x = Evaluate env y then 1 else 0
        | Position x -> env.[x]

    type Op =
        | Add = 1
        | Multiply = 2
        | Input = 3
        | Output = 4
        | IfTrue = 5
        | IfFalse = 6
        | LessThan = 7
        | Equal = 8
        | Stop = 99

    let GetMode m v =
        match m with
        | 0 -> Position v
        | 1 -> Immediate v
        | _ -> failwith $"Wrong mode={m}"

    let F1 (env: array<int>) (input: int list) =

        let rec Parse (env: array<int>) index input result =
            let instruction = env.[index]


            match enum (instruction % 100) with
            | Op.Stop -> result
            | x ->
                let mode1 =
                    GetMode (instruction / 100 % 10) env.[index + 1]

                let mode2 =
                    GetMode (instruction / 1000 % 10) env.[index + 2]

                match x with
                | Op.Add ->
                    env.[env.[index + 3]] <- Evaluate env (Add(mode1, mode2))
                    Parse env (index + 4) input result
                | Op.Multiply ->
                    env.[env.[index + 3]] <- Evaluate env (Multiply(mode1, mode2))
                    Parse env (index + 4) input result
                | Op.Input ->
                    match input with
                    | h :: t ->
                        env.[env.[index + 1]] <- h
                        Parse env (index + 2) t result
                    | _ -> failwith "do not have enough input"
                | Op.IfTrue ->
                    let next =
                        Evaluate env (IfTrue(mode1, mode2, index))

                    Parse env next input result
                | Op.IfFalse ->
                    let next =
                        Evaluate env (IfFalse(mode1, mode2, index))

                    Parse env next input result
                | Op.Equal ->
                    env.[env.[index + 3]] <- Evaluate env (Equal(mode1, mode2))
                    Parse env (index + 4) input result
                | Op.LessThan ->
                    env.[env.[index + 3]] <- Evaluate env (LessThan(mode1, mode2))
                    Parse env (index + 4) input result
                | Op.Output ->
                    let result = Evaluate env mode1
                    Parse env (index + 2) input result
                | _ -> failwith $"Wrong op=%A{instruction}"

        Parse env 0 input 0

[<Fact>]
let ``Day 5`` () =
    Day5.F1 (AocInput.Aoc2019.D5.Split(",") |> Array.map int) [ 1 ]
    |> should equal 4511442

    Day5.F1 (AocInput.Aoc2019.D5.Split(",") |> Array.map int) [ 5 ]
    |> should equal 12648139

module Day6 =
    let GetWorld input =
        input
        |> Array.groupBy fst
        |> Array.map (fun (k, v) -> k, v |> Array.map snd)
        |> Map.ofArray

    let GetRoot input =
        input
        |> Array.unzip
        |> (fun (x, y) -> Set.difference (x |> Set.ofArray) (y |> Set.ofArray))
        |> Set.toArray
        |> Array.head

    let F1 input =
        let world = GetWorld input
        let root = GetRoot input
        let result = world |> Map.map (fun _ _ -> 0)

        let rec GoThrough (world: Map<string, string []>) plant result =
            match world.TryFind plant with
            | Some plants ->
                let cur = (Map.find plant result) + 1

                plants
                |> Array.fold
                    (fun r p ->
                        let tmp = Map.add p cur r
                        GoThrough world p tmp)
                    result

            | None -> result

        GoThrough world root result
        |> Map.values
        |> Seq.sum

    let F2 input =
        let world = GetWorld input
        let root = GetRoot input

        let rec GetPath (world: Map<string, string []>) plant target path =
            match world.TryFind plant with
            | _ when plant = target -> Some path
            | None -> None
            | Some plants ->
                let path = plant :: path

                plants
                |> Array.tryPick (fun p -> GetPath world p target path)

        let you =
            GetPath world root "YOU" []
            |> Option.get
            |> List.rev

        let dst =
            GetPath world root "SAN" []
            |> Option.get
            |> List.rev

        let rec Remove you dst =
            match you, dst with
            | hy :: ty, hdst :: tdst when hy = hdst -> Remove ty tdst
            | _ -> List.length you + List.length dst

        Remove you dst

[<Fact>]
let ``Day 6`` () =
    Day6.F1(AocInput.Aoc2019.D6 |> Seq.toArray)
    |> should equal 453028

    Day6.F2(AocInput.Aoc2019.D6 |> Seq.toArray)
    |> should equal 562


module Day7 =
    let Amp program mode input =
        Day5.F1 (Array.copy program) [ mode; input ]

    let F1 program =
        let rec Calculate modes cur_modes =
            match List.length modes with
            | 0 ->
                let amps =
                    List.init 5 (fun _ -> Amp program)

                (cur_modes, amps)
                ||> List.fold2 (fun input m a -> a m input) 0
            | _ ->
                modes
                |> List.mapi (fun i m ->
                    let cur = m :: cur_modes
                    let modes = modes |> List.removeAt i
                    Calculate modes cur)
                |> List.max

        Calculate [ 0..4 ] List.empty

    open Day5

    type AmpState =
        | RUN of int
        | STOP of int

    let AmpCPU () =
        let mutable index = 0
        let mutable result = 0

        fun (env: array<int>) (input: int list) ->
            let rec Parse (env: array<int>) index input result =
                let instruction = env.[index]

                match enum (instruction % 100) with
                | Op.Stop -> STOP index, result
                | x ->
                    let mode1 =
                        GetMode (instruction / 100 % 10) env.[index + 1]

                    let mode2 =
                        GetMode (instruction / 1000 % 10) env.[index + 2]

                    match x with
                    | Op.Add ->
                        env.[env.[index + 3]] <- Evaluate env (Add(mode1, mode2))
                        Parse env (index + 4) input result
                    | Op.Multiply ->
                        env.[env.[index + 3]] <- Evaluate env (Multiply(mode1, mode2))
                        Parse env (index + 4) input result
                    | Op.Input ->
                        match input with
                        | h :: t ->
                            env.[env.[index + 1]] <- h
                            Parse env (index + 2) t result
                        | _ -> RUN index, result
                    | Op.IfTrue ->
                        let next =
                            Evaluate env (IfTrue(mode1, mode2, index))

                        Parse env next input result
                    | Op.IfFalse ->
                        let next =
                            Evaluate env (IfFalse(mode1, mode2, index))

                        Parse env next input result
                    | Op.Equal ->
                        env.[env.[index + 3]] <- Evaluate env (Equal(mode1, mode2))
                        Parse env (index + 4) input result
                    | Op.LessThan ->
                        env.[env.[index + 3]] <- Evaluate env (LessThan(mode1, mode2))
                        Parse env (index + 4) input result
                    | Op.Output ->
                        let result = Evaluate env mode1
                        //                    Parse env (index + 2) input result
                        RUN(index + 2), result
                    | _ -> failwith $"Wrong op=%A{instruction}"

            let i, r = Parse env index input result

            index <-
                match i with
                | RUN x -> x
                | STOP x -> x

            result <- r
            i, r


    let rec GetModes modes (cur_modes: int list) =
        match Set.count modes with
        | 0 -> [ cur_modes ]
        | _ ->
            modes
            |> Set.map (fun m ->
                let cur = m :: cur_modes
                let modes = Set.remove m modes
                GetModes modes cur)
            |> Set.toList
            |> List.concat

    let InitAmp (mode: int list) program =
        mode
        |> List.map (fun m ->
            let amp = AmpCPU () (Array.copy program)
            let i, r = amp [ m ]
            (amp, i, r))

    let rec Loop (end_index: int) amps (next_input: AmpState * int) =
        match next_input with
        | STOP _, r -> r
        | _ ->
            amps
            |> List.fold (fun (_, input) (amp, _, _) -> amp [ input ]) next_input
            |> Loop end_index amps

    let F2 program =

        let modes =
            GetModes ([| 5..9 |] |> Set.ofArray) List.empty

        modes
        |> List.map (fun m ->
            let init = InitAmp m program
            Loop -1 init (RUN 0, 0))
        |> List.max

[<Fact>]
let ``Day 7`` () =
    Day7.F1(AocInput.Aoc2019.D7.Split(",") |> Array.map int)
    |> should equal 21000

    Day7.F2(AocInput.Aoc2019.D7.Split(",") |> Array.map int)
    |> should equal 61379886

module Day8 =
    let F1 input =
        let layer = ((Seq.length input) / (25 * 6))

        input
        |> Seq.splitInto layer
        |> Seq.minBy (fun x ->
            x
            |> Seq.sumBy (function
                | 0 -> 1
                | _ -> 0))
        |> Seq.countBy id
        |> Seq.fold
            (fun s (k, v) ->
                match k with
                | 1
                | 2 -> s * v
                | _ -> s)
            1

    let F2 input =
        input
        |> Seq.indexed
        |> Seq.groupBy (fun (x, _) -> x % (25 * 6))
        |> Seq.map (fun (_, v) ->
            let _, r =
                v |> Seq.find (fun (_, y) -> y <> 2)

            r)

[<Fact>]
let ``Day 8`` () =
    AocInput.Aoc2019.D8
    |> Day8.F1
    |> should equal 2904

    AocInput.Aoc2019.D8
    |> Day8.F2
    |> Seq.map (fun x ->
        match x with
        | 1 -> "1"
        | 0 -> " "
        | _ -> " ")
    |> Seq.splitInto 6
    |> Seq.map (String.concat "")
    |> Seq.iter (fun x -> printfn "%A" x)

    printfn "Aoc2019.D8 F2 result: HGBCF"
