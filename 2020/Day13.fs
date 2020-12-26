namespace Year2020

module Day13 =
    open Utilities
    open System

    let parser (input: string) =
        match input.Split('\n') with
        | [| arrivalTimestamp; busLines |] -> 
            let arrivalTimestamp = int64 arrivalTimestamp
            arrivalTimestamp, 
                busLines.Split(',') 
                |> Array.mapi (fun i b -> match Int64.TryParse b with true, x -> Some (int64 i, x) | _ -> None)
                |> Array.choose id
                |> Seq.ofArray
        | _ -> failwith "Invalid input"

    let getEarliestArrival arrivalTimestamp busLines =
        Seq.map (fun (i, value) -> value, (i, value - (arrivalTimestamp % value))) busLines

    let arrivalTimestamp, lines = getSingle 2020 13 parser
    let busLinesWithEarliestArrivals x = getEarliestArrival x lines |> Seq.map snd

    let rec calculateTimestamp idx timestamp step busLine =
        if (timestamp + idx) % busLine = 0L 
        then timestamp, step * busLine
        else calculateTimestamp idx (timestamp + step) step busLine

    let part1() = 
        getEarliestArrival arrivalTimestamp lines 
        |> Seq.map (fun (id, (_, busLine)) -> id, busLine) 
        |> Seq.sortBy snd 
        |> Seq.head 
        |> fun (id, minutesWaiting) -> id * minutesWaiting
    
    let part2() =
        let busLines = busLinesWithEarliestArrivals 0L 
        let _, firstBusLine = Seq.head busLines
        Seq.skip 1 busLines
        |> Seq.fold (fun (timestamp, step) (idx, value) -> calculateTimestamp idx timestamp step value) (0L, firstBusLine)
        |> fst

    let solve () = printDay 2020 13 part1 part2
    