namespace Year2020
open System.Collections.Generic

module Day12 =
    open Utilities

    type Direction = North | South | East  | West

    type Instruction =
        | North of int | South of int
        | East of int  | West of int
        | Left of int  | Right of int
        | Forward of int

    let parser = function
        | Regex "^([A-Z])([0-9]+)$" [dir; value] ->
            let value = int value 
            match dir with 
            | "N" -> North value   | "S" -> South value
            | "E" -> East value    | "W" -> West value
            | "L" -> Left value    | "R" -> Right value
            | "F" -> Forward value 
            | _ -> failwith "Unknown direction"
        | _ -> failwith "Unknown instruction" 

    let instructions = getMany 2020 12 parser |> List.ofSeq

    let manhattandistance = fun (a, b) (c, d) ->  abs (a - c) + abs (b - d)

    let moveForward position direction value =
        let (x, y) = position
        match direction with 
        | Direction.North -> (x, y - value) | Direction.South -> (x, y + value) 
        | Direction.East  -> (x + value, y) | Direction.West  -> (x - value, y)

    let toAngle = function
        | Direction.North -> 0 | Direction.East  -> 90
        | Direction.South -> 180 | Direction.West  -> 270

    let toDirection = function
        | 0     -> Direction.North | 90    -> Direction.East
        | 180   -> Direction.South | 270   -> Direction.West
        | x -> failwithf "Invalid value: %d" x

    let turn direction instruction =
        let angle = toAngle direction
        match instruction with
        | Right degrees -> toDirection ((angle + degrees) % 360)
        | Left degrees  -> toDirection ((360 + ((angle - degrees))) % 360)
        | x -> failwithf "Only Right and Left are valid, you provided: '%A'" x

    let rec move position direction = function
        | [] -> manhattandistance (0, 0) position
        | instruction :: xs ->
            let (x, y) = position
            match instruction with
            | North value   -> move (x, y - value) direction xs
            | South value   -> move (x, y + value) direction xs
            | East value    -> move (x + value, y) direction xs
            | West value    -> move (x - value, y) direction xs
            | Left value    -> move position (turn direction (Left value)) xs
            | Right value   -> move position (turn direction (Right value)) xs
            | Forward value -> move (moveForward position direction value) direction xs

    let part1() = move (0, 0) Direction.East instructions
    let part2() = 0

    let solve () = printDay 2020 12 part1 part2
