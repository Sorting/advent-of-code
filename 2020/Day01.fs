namespace Year2020

module Day01 =
    open Utilities

    let numbers = getMany 2020 1 int |> Array.ofSeq
    
    let part1() =
        let length = Array.length numbers - 1
        seq { for x in 0..length do            
                for y in x..length -> 
                    numbers.[x] + numbers.[y], numbers.[x] * numbers.[y] }
        |> Seq.pick (function
            | (k, v) when k = 2020 -> Some v
            | _ -> None)

    let part2() =
        let length = Array.length numbers - 1
        [ for x in 0..length do            
            for y in x..length do
                for z in y..length ->
                    numbers.[x] + numbers.[y] + numbers.[z], numbers.[x] * numbers.[y] * numbers.[z] ]
        |> Seq.pick (function
            | (k, v) when k = 2020 -> Some v
            | _ -> None)

    let solve () = printDay 2020 1 part1 part2
