namespace Year2020

module Day05 =
    open Utilities

    let parser (input: string) = input.ToCharArray() |> List.ofArray
    let instructions = getMany 2020 5 parser |> List.ofSeq
    let split f l = f (List.length l / 2) l

    let rec getSeatId rows columns = function
        | [] -> List.head rows * 8 + List.head columns
        | x :: xs -> 
            match x with
            | 'F'   -> getSeatId (split List.take rows) columns xs
            | 'B'   -> getSeatId (split List.skip rows) columns xs
            | 'L'   -> getSeatId rows (split List.take columns) xs
            | _     -> getSeatId rows (split List.skip columns) xs

    let seatIds = 
        instructions 
        |> List.map (getSeatId [0..127] [0..7]) 
        |> List.sort

    let getMySeatId =
        seq { for i in 1..127 ->
                if seatIds.[i + 1] - seatIds.[i - 1] = 2 then Some (seatIds.[i - 1] + 1) else None }
        |> Seq.pick id
    
    let part1() = List.last seatIds
    let part2() = getMySeatId

    let solve () = printDay 2020 5 part1 part2
