module Day06

open Utilities

type Location =
| Coordinate of int * int
| Fence

let coordinates = Set.ofSeq (getMany 6 (fun x -> 
    match x.Split(',') with
    | [| x; y |] -> int x, int (y.Trim())
    | _ -> 0, 0))

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2 + y1 - y2)

let part1() = 
    let (minX, minY) = coordinates |> Seq.min
    let (maxX, maxY) = Seq.max coordinates

    let matrix =
        [ for y in minY..maxY do
            yield! 
                [ for x in minX..maxX -> 
                    let group = 
                        Seq.map (fun coordinate -> manhattanDistance (x, y) coordinate, coordinate) coordinates
                        |> Seq.filter (fun (distance, _) -> distance > 0)
                        |> Seq.groupBy (fun (distance, _) -> distance)

                    // if Seq.exists (fun (_, v) -> (Seq.length v) > 1) group
                    // then (x, y), Fence
                    // else
                    Seq.minBy (fun (key, _) -> key) group
                    |> fun (_, v) -> (x, y), Seq.head v |> fun (_, (x, y)) -> Coordinate(x, y)
                ] ]
    matrix 
    |> Seq.choose (fun (_, location) -> match location with Coordinate(x, y) -> Some (x, y) | Fence -> None) 
    |> Seq.groupBy (id)
    |> Seq.length

    
let part2() = manhattanDistance (0, 0) (201, 235)

let solve() = printDay 6 part1 part2