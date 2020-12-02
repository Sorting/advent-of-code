namespace Year2020

module Day01 =
    open Utilities
    open AdventOfCode

    let numbers = getMany 2020 1 int
    
    let part1() =
        numbers
        |> Seq.collect(fun x -> numbers |> Seq.map (fun y -> x + y, (x, y)))
        |> Map.ofSeq
        |> Map.tryFind 2020
        |> function
            | Some (x, y) -> x * y
            | None -> failwith "Didn't find what we were looking for"

    let part2() = 
        numbers
        |> Seq.collect(fun x -> numbers |> Seq.collect (fun y -> numbers |> Seq.map (fun z -> x + y + z, (x, y, z))))
        |> Map.ofSeq
        |> Map.tryFind 2020
        |> function
            | Some (x, y, z) -> x * y * z
            | None -> failwith "Didn't find what we were looking for"

    let solve () = printDay 2020 1 part1 part2
