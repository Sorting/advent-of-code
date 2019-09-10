// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let solutions = 
        [ ((2015, 7), Year2015.Day07.solve)
          ((2018, 1), Year2018.Day01.solve); ((2018, 2), Year2018.Day02.solve); ((2018, 3), Year2018.Day03.solve) 
          ((2018, 4), Year2018.Day04.solve); ((2018, 5), Year2018.Day05.solve); ((2018, 6), Year2018.Day06.solve)
        ] |> Map.ofList

    match argv |> List.ofArray with
    | [] -> Map.iter (fun _ solve -> solve()) solutions
    | year::days ->
        let year = int year
        days |> List.iter (fun day ->
            match Int32.TryParse day with
            | true, day when day > 0 && day < 26 -> 
                match Map.tryFind (year, day) solutions with
                | Some solve -> solve()
                | _ -> printfn "Day %d hasn't been implemented yet" day
            | _ -> printfn "Invalid argument, day must be an integer between 1-25")
        

    0 // return an integer exit code
