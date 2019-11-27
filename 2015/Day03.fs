namespace Year2015

module Day03 =
    open Utilities

    type Direction = North | South | East | West

    let parseDirection (characters: string) =
        characters.ToCharArray()
        |> List.ofArray
        |> List.map (function
            | '^' -> North
            | '>' -> East 
            | 'v' -> South
            | _   -> West)

    let directions = getSingle 2015 3 (string >> parseDirection)

    let rec move map (x, y) directions =
        let map' = Set.add (x, y) map
        match directions with
        | [] -> map'
        | head::tail ->
            let position = 
                match head with
                | North -> (x, y+1)
                | East -> (x+1, y)
                | South -> (x, y-1)
                | West -> (x-1, y)
            move map' position tail

    let rec splitList list =
        let rec split odd even = function
            | a::b::tail -> split (odd @ [a]) (even @ [b]) tail
            | a::tail -> split (odd @ [a]) even tail
            | [] -> odd, even
        split [] [] list

    let part1() = move Set.empty (0, 0) directions |> Set.count
    
    let part2() =
        let santaDirections, robotDirections = splitList directions
        let santaMap, robotMap = 
            move Set.empty (0, 0) santaDirections,
            move Set.empty (0, 0) robotDirections
        Set.union robotMap santaMap |> Set.count
    
    let solve() = printDay 2015 3 part1 part2