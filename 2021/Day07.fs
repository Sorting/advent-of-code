namespace Year2021

module Day07 =
    open Utilities

    let parser (input: string) =
        input.Split(',') 
        |> Array.map int 
        |> Array.sort
        |> List.ofArray

    let positions = getSingle 2021 7 parser
    
    let fuelFriendlyPosition positions =
        positions
        |> List.map (fun p -> 
            positions
            |> List.sumBy (fun p' -> abs (p - p')))
        |> List.min

    let crabEngineeredFuelFriendlyPosition position =
        positions
        |> List.map (fun p -> 
            positions
            |> List.sumBy (fun p' -> 
                let diff = abs (p + 1 - p')
                [1..diff] |> List.sum))
        |> List.min

    let part1() = fuelFriendlyPosition positions
    let part2() = crabEngineeredFuelFriendlyPosition positions

    let solve () = printDay 2021 7 part1 part2