// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let solutions = 
        Map.ofList 
            [ ((2015, 1), Year2015.Day01.solve); ((2015, 2), Year2015.Day02.solve); ((2015, 3), Year2015.Day03.solve); 
              ((2015, 4), Year2015.Day04.solve); ((2015, 5), Year2015.Day05.solve); ((2015, 7), Year2015.Day07.solve); 
              ((2015, 13), Year2015.Day13.solve)
              ((2017, 3), Year2017.Day03.solve)
              ((2018, 1), Year2018.Day01.solve); ((2018, 2), Year2018.Day02.solve); ((2018, 3), Year2018.Day03.solve) 
              ((2018, 4), Year2018.Day04.solve); ((2018, 5), Year2018.Day05.solve); ((2018, 6), Year2018.Day06.solve)
              ((2019, 1), Year2019.Day01.solve); ((2019, 2), Year2019.Day02.solve); ((2019, 3), Year2019.Day03.solve)
              ((2019, 4), Year2019.Day04.solve); ((2019, 5), Year2019.Day05.solve); ((2019, 6), Year2019.Day06.solve)
              ((2019, 7), Year2019.Day07.solve); ((2019, 8), Year2019.Day08.solve); ((2019, 9), Year2019.Day09.solve)
              ((2019, 10), Year2019.Day10.solve)
              ((2020, 1), Year2020.Day01.solve); ((2020, 2), Year2020.Day02.solve); ((2020, 3), Year2020.Day03.solve)
              ((2020, 4), Year2020.Day04.solve); ((2020, 5), Year2020.Day05.solve); ((2020, 6), Year2020.Day06.solve)
              ((2020, 7), Year2020.Day07.solve); ((2020, 8), Year2020.Day08.solve); ((2020, 9), Year2020.Day09.solve)
            ]

    match argv |> List.ofArray with
    | [] -> Map.iter (fun _ solve -> solve()) solutions
    | [year] ->
        let year' = int year
        solutions
        |> Map.filter (fun (year, _) _ -> year = year')
        |> Map.iter (fun _ solve -> solve())
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
