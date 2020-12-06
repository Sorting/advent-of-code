namespace Year2020

module Day06 =
    open System
    open Utilities

    let parser (input: string) =
        input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun groups ->
            groups.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun answers -> answers.ToCharArray()))

    let groupAnswers = getSingle 2020 6 parser
    
    let part1() = 
        groupAnswers 
        |> Array.map (Array.collect id >> Set.ofArray)
        |> Array.fold (fun acc x -> acc + Set.count x) 0
    
    let part2() =
        groupAnswers 
        |> Array.map (fun members -> 
            members 
            |> Array.collect id 
            |> Array.countBy id 
            |> Array.filter (fun (_, count) -> count = Array.length members)
            |> Array.length)
        |> Array.reduce (+)

    let solve () = printDay 2020 6 part1 part2
