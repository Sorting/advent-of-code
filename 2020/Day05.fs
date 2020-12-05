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
        |> Set.ofList

    let getMySeatId =
        seq { for row in 0..128 do
                yield! seq { for column in 0..8 ->
                                let currentId = row * 8 + column
                                if  not (Set.contains currentId seatIds) && 
                                    Set.contains (currentId + 1) seatIds &&
                                    Set.contains (currentId - 1) seatIds
                                then Some currentId
                                else None } }
        |> Seq.pick id
    
    let part1() = Set.maxElement seatIds
    let part2() = getMySeatId

    let solve () = printDay 2020 5 part1 part2
