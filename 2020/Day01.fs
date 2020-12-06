namespace Year2020

module Day01 =
    open Utilities
    open AdventOfCode

    let numbers = getMany 2020 1 int |> Array.ofSeq
    
    let part1() =
        let length = Array.length numbers - 1
        [ for x in 0..length do            
            for y in x..length -> 
                numbers.[x] + numbers.[y], (numbers.[x], numbers.[y]) ]
        |> Map.ofList
        |> Map.tryFind 2020
        |> function
            | Some (x, y) -> x * y
            | None -> failwith "Didn't find what we were looking for"

    let part2() =
        let length = Array.length numbers - 1
        [ for x in 0..length do            
            for y in x..length do
                for z in y..length ->
                    numbers.[x] + numbers.[y] + numbers.[z], (numbers.[x], numbers.[y], numbers.[z]) ]
        |> Map.ofList
        |> Map.tryFind 2020
        |> function
            | Some (x, y, z) -> x * y * z
            | None -> failwith "Didn't find what we were looking for"

    let solve () = printDay 2020 1 part1 part2
