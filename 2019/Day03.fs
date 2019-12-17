namespace Year2019

module Day03 =
    open Utilities

    type Direction = Left of int | Right of int | Up of int | Down of int | Unknown
    type Position = IntersectionPoint of int * int | Point of WireType * int * int 
    and WireType = FirstWire | SecondWire

    let parse (directionString : string) =
        directionString.Split(',')
        |> Array.map (function
            | Regex @"^R(\d+)$" [distance] -> Right (int distance)
            | Regex @"^L(\d+)$" [distance] -> Left (int distance)
            | Regex @"^U(\d+)$" [distance] -> Up (int distance)
            | Regex @"^D(\d+)$" [distance] -> Down (int distance)
            | _                            -> Unknown)
        |> Array.toList

    let paths = getMany 2019 3 parse |> Seq.toList
    let manhattanDistance = fun (a, b) (c, d) -> abs (a - c) + abs (b - d)

    let rec buildGrid map currentPosition steps wireType =
        function
        | [] -> map
        | direction::xs ->
            let x, y = currentPosition
            let newPositions = [
                match direction with
                | Up distance    -> yield! [for i in 1..distance -> x, y-i, steps+i]
                | Down distance  -> yield! [for i in 1..distance -> x, y+i, steps+i]
                | Left distance  -> yield! [for i in 1..distance -> x-i, y, steps+i]
                | Right distance -> yield! [for i in 1..distance -> x+i, y, steps+i]
                | Unknown        -> failwith "Something went wrong!" ]
            let map' =                        
                List.fold (fun acc (x, y, currentSteps) ->                    
                    match Map.tryFind (x, y) acc with
                    | Some (Point (wt, firstWireSteps, secondWireSteps)) when wt <> wireType -> 
                        let steps' = (if wireType = FirstWire then (currentSteps, secondWireSteps) else (firstWireSteps, currentSteps))
                        acc |> Map.remove (x, y) |> Map.add (x, y) (IntersectionPoint steps')
                    | None -> 
                        let firstWireSteps, secondWireSteps = if wireType = FirstWire then (currentSteps, 0) else (0, currentSteps)
                        acc |> Map.add (x, y) (Point(wireType,firstWireSteps,secondWireSteps))
                    | _ ->
                        acc) map newPositions

            let x, y, currentSteps = List.last newPositions
            let nextPosition = x, y
            match Map.tryFind nextPosition map' with
            | Some (Point (wt, firstWireSteps, secondWireSteps)) when wt <> wireType -> 
                let steps' = if wireType = FirstWire then (currentSteps, secondWireSteps) else (firstWireSteps, currentSteps)
                buildGrid (map' |> Map.remove nextPosition |> Map.add nextPosition (IntersectionPoint steps')) nextPosition currentSteps wireType xs
            | _ -> 
                let x, y = if wireType = FirstWire then (currentSteps, 0) else (0, currentSteps)
                buildGrid (map' |> Map.add nextPosition (Point(wireType, x, y))) nextPosition currentSteps wireType xs

    let part1() = 
        buildGrid Map.empty (0, 0) 0 FirstWire (paths.[0])
        |> fun map -> buildGrid map (0, 0) 0 SecondWire (paths.[1])
        |> Map.filter (fun _ x -> match x with IntersectionPoint _ -> true | _ -> false)
        |> Map.toList
        |> List.map (fun (pos, _) -> manhattanDistance (0, 0) pos)
        |> List.min

    let part2() =
        buildGrid Map.empty (0, 0) 0 FirstWire (paths.[0])
        |> fun map -> buildGrid map (0, 0) 0 SecondWire (paths.[1])
        |> Map.filter (fun _ x -> match x with IntersectionPoint _ -> true | _ -> false)
        |> Map.toList
        |> List.map (fun (_, x) -> match x with IntersectionPoint(p1, p2) -> p1+p2 | _ -> 0)
        |> List.min

    let solve() = printDay 2019 3 part1 part2