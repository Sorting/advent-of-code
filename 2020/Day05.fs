namespace Year2020

module Day05 =
    open Utilities

    let parser (input: string) = input.ToCharArray() |> List.ofArray
    let instructions = getMany 2020 5 parser |> List.ofSeq
    let seats = Array2D.create 128 8 0

    let rec getSeatId rows columns = function
        | [] -> List.head rows * 8 + List.head columns
        | x :: xs -> 
            match x with
            | 'F'   -> getSeatId (rows |> List.take (List.length rows / 2)) columns xs
            | 'B'   -> getSeatId (rows |> List.skip (List.length rows / 2)) columns xs
            | 'L'   -> getSeatId rows (columns |> List.take (List.length columns / 2)) xs
            | _     -> getSeatId rows (columns |> List.skip (List.length columns / 2)) xs

    let seatIds = 
        instructions 
        |> List.map (getSeatId [0..127] [0..7]) 
        |> Set.ofList

    let getMySeatId =
        seq { for row in 0..128 do
                yield! 
                    seq { for column in 0..8 do
                            let currentId = row * 8 + column
                            if  not (Set.contains currentId seatIds) && 
                                Set.contains (currentId + 1) seatIds &&
                                Set.contains (currentId - 1) seatIds
                            then yield Some currentId
                            else yield None } }
        |> Seq.pick id
    
    let part1() = Set.maxElement seatIds
    let part2() = getMySeatId

    let solve () = printDay 2020 5 part1 part2
