namespace Year2020

module Day11 =
    open Utilities

    type Seat = Floor | Empty | Occupied
    type Rule = DirectAdjacents | AdjacentsInShight
    type Direction = W | NW | N | NE | E | SE | S | SW

    let parser y (line: string) =
        line.ToCharArray()
        |> Array.mapi (fun x c -> (x, y), match c with '.' -> Floor | '#' -> Occupied | _ -> Empty)
        |> List.ofArray
        
    let map = 
        getMany 2020 11 string 
        |> Seq.mapi parser 
        |> Seq.collect id
        |> Map.ofSeq

    let rec check rule lookingFor position map direction =
        let x, y = position 
        let nextPosition = 
            match direction with
            | W  -> x-1, y | NW -> x-1, y-1 | N  -> x, y-1 | NE -> x+1, y-1
            | E  -> x+1, y | SE -> x+1, y+1 | S  -> x, y+1 | SW -> x-1, y+1
        match Map.tryFind nextPosition map with
        | Some nextSeat ->
            match rule with
            | DirectAdjacents ->
                match lookingFor with
                | Empty -> nextSeat = Empty || nextSeat = Floor
                | _ -> nextSeat = Occupied
            | _ ->
                match lookingFor, nextSeat with
                | Empty, Empty       -> true
                | Occupied, Occupied -> true
                | Empty, Occupied    -> false
                | Occupied, Empty    -> false
                | _ -> check rule lookingFor nextPosition map direction
        | _ -> lookingFor = Empty

    let rec reachEquilibrium rule n map =
        let map' = Map.map (fun position seat -> 
            let directions = [ W; NW; N; NE; E; SE; S; SW]
            match seat with
            | Empty -> 
                directions 
                |> List.forall (check rule Empty position map)
                |> function true -> Occupied | _ -> Empty
            | Occupied ->
                directions
                |> List.filter (check rule Occupied position map)
                |> List.length
                |> fun length -> if length >= n then Empty else Occupied
            | _ -> Floor ) map
        if map <> map'
        then reachEquilibrium rule n map'
        else map |> Map.filter (fun _ t -> t = Occupied) |> Map.count

    let part1() = reachEquilibrium DirectAdjacents 4 map 
    let part2() = reachEquilibrium AdjacentsInShight 5 map

    let solve () = printDay 2020 11 part1 part2