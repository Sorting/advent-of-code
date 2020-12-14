namespace Year2020
open System.Text
open System.Security.Cryptography
open System

module Day11 =
    open Utilities

    type Seat = Floor | Empty | Occupied

    let parser y (line: string) =
        line.ToCharArray()
        |> Array.mapi (fun x c -> (x, y), match c with '.' -> Floor | _ -> Occupied)
        |> List.ofArray
        
    let map = 
        getMany 2020 11 string 
        |> Seq.mapi parser 
        |> Seq.collect id
        |> Map.ofSeq

    let checkPos seat map pos  =
        match Map.tryFind pos map with
        | Some seat' -> if seat = Empty then seat' = Empty || seat' = Floor else seat' = Occupied
        | _ -> seat = Empty

    let rec sol map =
        let map' = 
            map 
            |> Map.map (fun (x, y) seat -> 
                let adjecents = 
                    [ (x, y-1); (x-1, y); (x, y+1); (x+1, y)
                      (x-1, y-1); (x-1, y+1); (x+1, y+1); (x+1, y-1)]
                match seat with
                | Empty -> 
                    adjecents 
                    |> List.forall (checkPos Empty map)
                    |> function true -> Occupied | _ -> seat
                | Occupied ->
                    adjecents
                    |> List.filter (checkPos Occupied map)
                    |> List.length
                    |> fun length -> if length > 3 then Empty else seat
                | _ -> seat)
        
        if map <> map'
        then sol map'
        else map

    let part1() = sol map |> Map.filter (fun _ t -> t = Occupied) |> Map.count
    let part2() = 0

    let solve () = printDay 2020 11 part1 part2
