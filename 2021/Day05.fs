namespace Year2021

module Day05 =
    open Utilities

    type Segment = int * int * int * int

    let parser = function
        | Regex @"^(\d+),(\d+) -> (\d+),(\d+)$" [x1; y1; x2; y2] -> Segment (int x1, int y1, int x2, int y2)
        | _ -> failwith "invalid segment"

    let segments = getMany 2021 5 parser |> Seq.toList
    let toPositive x = if x < 0 then 360 + x else x
    let calculateAngle (x1, y1, x2, y2) = int (atan2 (float y2 - float y1) (float x2 - float x1) * 180. / System.Math.PI) + 90 |> toPositive
    
    let drawLine range checkDiagonal map  =
        let x1, y1, x2, y2 = range
        let rec aux map = function
            | [] -> map
            | (x, y) :: xs ->
                let key = x, y
                let map' = 
                    match Map.tryFind key map with
                    | Some value -> Map.add key (value + 1) map
                    | None -> Map.add key 1 map
                aux map' xs

        let list =
            match calculateAngle range, checkDiagonal with
            | 0, _      -> List.zip [for _ in y2..y1 -> x1] (List.rev [y2..y1])
            | 45, true  -> List.zip [x1..x2] (List.rev [y2..y1])
            | 90, _     -> List.zip [x1..x2] [for _ in x1..x2 -> y1]
            | 135, true -> List.zip [x1..x2] [y1..y2]
            | 180, _    -> List.zip [for _ in y1..y2 -> x1] [y1..y2]
            | 225, true -> List.zip (List.rev [x2..x1]) [y1..y2]
            | 270, _    -> List.zip [x2..x1] [for _ in x2..x1 -> y1]
            | 315, true -> List.zip (List.rev [x2..x1]) (List.rev [y2..y1])
            | _         -> []
        aux map list
    
    let rec buildMap map checkDiagonal =
        function
        | [] -> map
        | range :: xs -> buildMap (drawLine range checkDiagonal map) checkDiagonal xs

    let overlapCount checkDiagonal =
        buildMap Map.empty checkDiagonal segments
        |> Map.filter (fun _ value -> value > 1)
        |> Map.count
        
    let part1() = overlapCount false
    let part2() = overlapCount true

    let solve () = printDay 2021 5 part1 part2