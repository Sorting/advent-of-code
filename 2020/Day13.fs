namespace Year2020

module Day13 =
    open Utilities
    open System

    let parser (input: string) =
        match input.Split('\n') with
        | [| arrivalTimestamp; busLines |] -> 
            let arrivalTimestamp = int arrivalTimestamp
            arrivalTimestamp, 
            busLines.Split(',')
            |> Seq.ofArray 
            |> Seq.choose (fun x ->
                match Int32.TryParse(x) with
                | true, value -> Some (value, value - (arrivalTimestamp % value))
                | _ -> None)
            |> Seq.sortBy snd
        | _ -> failwith "Invalid input"
    
    let arrivalTimestamp, lines = getSingle 2020 13 parser
    
    let part1() = lines |> Seq.head |> fun (id, watingMinutes) -> id * watingMinutes
    let part2() = 0

    let solve () = printDay 2020 13 part1 part2
