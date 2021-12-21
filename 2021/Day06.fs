namespace Year2021

module Day06 =
    open Utilities
    let parser (x: string) =
        x.Split(',') 
        |> Array.map int 
        |> Array.countBy id
        |> Array.map (fun (a, b) -> a, int64 b)
        |> Map.ofArray

    let numbers = getSingle 2021 6 parser
    
    let rec turn daysToRun daysPassed fishes = 
        if daysPassed = daysToRun 
        then Map.toList fishes |> List.sumBy snd
        else
            let rec aux prevValue key m =
                let currentValue = 
                    match Map.tryFind key m with 
                    | Some value -> value
                    | _ -> 0L
                let m = Map.add key prevValue m
                if key > 0
                then aux currentValue (key - 1) m 
                else
                    let m = Map.add 8 currentValue m
                    let sv =
                        match Map.tryFind 6 m with
                        | Some value -> value + currentValue
                        | None -> currentValue
                    Map.add 6 sv m
            turn daysToRun (daysPassed + 1) (aux 0L 8 fishes)

    let part1() = turn 80 0 numbers
    let part2() = turn 256 0 numbers
    
    let solve () = printDay 2021 6 part1 part2