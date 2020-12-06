namespace Year2020

module Day06 =
    open System
    open Utilities

    let parser (input: string) =
        input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun groups ->
            groups.Split([| '\n'; ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun answers -> answers.ToCharArray() |> List.ofArray)            
            |> List.ofArray)
        |> List.ofArray

    let groupAnswers = getSingle 2020 6 parser
    
    let part1() = 
        groupAnswers 
        |> List.map (List.collect id >> Set.ofList)
        |> List.fold (fun acc x -> acc + Set.count x) 0
    
    let part2() =
        groupAnswers 
        |> List.map (fun members -> 
            members 
            |> List.collect id 
            |> List.countBy id 
            |> List.filter (fun (_, count) -> count = List.length members) 
            |> List.length)
        |> List.reduce (+)

    let solve () = printDay 2020 6 part1 part2
