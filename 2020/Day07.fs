namespace Year2020

module Day07 =
    open System.Text.RegularExpressions
    open Utilities

    let parser (line: string) =
        let split = line.Split(' ')
        let color = sprintf "%s %s" split.[0] split.[1]
        let matches = Regex.Matches(line, "(?<n>[0-9]) (?<color>[a-z]+ [a-z]+)")
        color, matches |> Seq.map (fun m -> m.Groups.["color"].Value, int (m.Groups.["n"].Value)) |> Map.ofSeq

    let rec hasBag bag key bagMap =
        match Map.tryFind key bagMap with
        | Some bags -> 
            if Map.containsKey bag bags then true
            else Map.exists (fun key _ -> hasBag bag key bagMap) bags
        | None -> false

    let rec countBags bag total bagMap =
        match Map.tryFind bag bagMap with
        | Some bags -> 
            total + 1 + (bags 
            |> Map.toList 
            |> List.sumBy (fun (key, bagCount) -> countBags key total bagMap * bagCount))
        | None -> total

    let bagMap = getMany 2020 7 parser |> Map.ofSeq
    
    let part1() = Map.filter (fun key _ -> hasBag "shiny gold" key bagMap) bagMap |> Map.count
    let part2() = countBags "shiny gold" 0 bagMap - 1

    let solve () = printDay 2020 7 part1 part2
