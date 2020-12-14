namespace Year2020
open System.Text
open System.Security.Cryptography
open System

module Day11 =
    open Utilities

    type Item = Floor | Empty | Occupied

    let parser y (line: string) =
        line.ToCharArray()
        |> Array.mapi (fun x c -> (x, y), match c with '.' -> Floor | _ -> Occupied)
        |> List.ofArray
        
    let map = 
        getMany 2020 11 string 
        |> Seq.mapi parser 
        |> Seq.collect id
        |> Map.ofSeq

    let tester t map pos  =
        match Map.tryFind pos map with
        | Some t' -> if t = Empty then t' = t || t' = Floor else t' = t
        | _ -> t = Empty

    let rec sol map =
        let map' = 
            map 
            |> Map.map (fun (x, y) t -> 
                let adjecents = 
                    [ (x, y-1); (x-1, y); (x, y+1); (x+1, y)
                      (x-1, y-1); (x-1, y+1); (x+1, y+1); (x+1, y-1)]
                match t with
                | Empty -> 
                    adjecents 
                    |> List.forall (tester Empty map)
                    |> function true -> Occupied | _ -> t
                | Occupied ->
                    adjecents
                    |> List.filter (tester Occupied map)
                    |> List.length
                    |> fun length -> if length > 3 then Empty else t
                | _ -> t)
        
        if map <> map'
        then sol map'
        else map

    let part1() = sol map |> Map.filter (fun _ t -> t = Occupied) |> Map.count
    let part2() = 0

    let solve () = printDay 2020 11 part1 part2
