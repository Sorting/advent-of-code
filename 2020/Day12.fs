namespace Year2020

module Day12 =
    open Utilities

    type Direction = North | South | East  | West
    type Rotate = Clockwise | CounterClockwise
    type MoveType = Ship | Waypoint

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
    let manhattanDistance = fun (a, b) (c, d) ->  abs (a - c) + abs (b - d)

    let moveForward moveType waypoint ship direction value =
        let wx, wy = waypoint
        let sx, sy = ship
        match moveType with
        | Ship ->
            match direction with 
            | Direction.North -> wx, wy + value | Direction.South -> wx, wy - value 
            | Direction.East  -> wx + value, wy | Direction.West  -> wx - value, wy
        | Waypoint -> sx + (wx * value), sy + (wy * value)

    let toDegree = function
        | Direction.North -> 0   | Direction.East -> 90
        | Direction.South -> 180 | Direction.West -> 270

    let toDirection = function
        | 0     -> Direction.North | 90  -> Direction.East
        | 180   -> Direction.South | 270 -> Direction.West
        | x -> failwithf "Invalid value: %d" x

    let rotate position instruction = 
        let rotate90 position orientation = 
            let x, y = position
            match orientation with
            | Clockwise _ -> y, -x
            | CounterClockwise -> -y, x
        
        let formula degrees orientation = 
            [ 1..degrees/90] 
            |> List.fold (fun acc _ -> rotate90 acc orientation) position
        
        match instruction with
        | Right degrees -> formula degrees Clockwise
        | Left degrees  -> formula degrees CounterClockwise
        | x -> failwithf "Invalid instruction: '%A'" x  

    let turn moveType waypoint direction instruction =
        let waypoint =
            match moveType with
            | Ship     -> waypoint
            | Waypoint -> rotate waypoint instruction
        let angle = toDegree direction
        match instruction with
        | Right degrees -> toDirection ((angle + degrees) % 360), waypoint
        | Left degrees  -> toDirection ((360 + angle - degrees) % 360), waypoint
        | x -> failwithf "Invalid instruction: '%A'" x

    let rec move moveType waypoint ship direction = function
        | [] -> 
            match moveType with
            | Ship     -> manhattanDistance (0, 0) waypoint
            | Waypoint -> manhattanDistance (0, 0) ship
        | instruction :: xs ->
            let x, y = waypoint
            match instruction with
            | North value   -> move moveType (x, y + value) ship direction xs
            | South value   -> move moveType (x, y - value) ship direction xs
            | East value    -> move moveType (x + value, y) ship direction xs
            | West value    -> move moveType (x - value, y) ship direction xs
            | Left _ | Right _ -> 
                let direction, waypoint = turn moveType waypoint direction instruction
                move moveType waypoint ship direction xs
            | Forward value -> 
                match moveType with
                | Ship     -> move Ship (moveForward moveType waypoint ship direction value) ship direction xs
                | Waypoint -> move Waypoint waypoint (moveForward moveType waypoint ship direction value) direction xs

    let part1() = move Ship (0, 0) (0, 0) Direction.East instructions
    let part2() = move Waypoint (10, 1) (0, 0) Direction.East instructions

    let solve () = printDay 2020 12 part1 part2