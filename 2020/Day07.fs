namespace Year2020

module Day07 =
    open System.Text.RegularExpressions
    open Utilities

    let parser (line: string) =
        let split = line.Split(' ')
        let color = sprintf "%s %s" split.[0] split.[1]
        let matches = Regex.Matches(line, "(?<n>[0-9]) (?<color>[a-z]+ [a-z]+)")
        color, matches |> Seq.map (fun m -> m.Groups.["color"].Value, int (m.Groups.["n"].Value)) |> Map.ofSeq

    let rec hasBag bag key map =
        match Map.tryFind key map with
        | Some neighbours -> 
            if Map.containsKey bag neighbours then true
            else Map.exists (fun key _ -> hasBag bag key map) neighbours
        | None -> false

    let rec countBags bag count map =
        match Map.tryFind bag map with
        | Some bags -> 
            count + 1 + (bags 
            |> Map.toList 
            |> List.sumBy (fun (key, numBags) -> countBags key count map * numBags))
        | None -> count

    let bagMap = getMany 2020 7 parser |> Map.ofSeq
    
    let part1() = Map.filter (fun key _ -> hasBag "shiny gold" key bagMap) bagMap |> Map.count
    let part2() = countBags "shiny gold" 0 bagMap - 1

    let solve () = printDay 2020 7 part1 part2
