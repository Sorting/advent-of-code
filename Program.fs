// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let solutions = 
        [ 
          (1, Day01.solve); (2, Day02.solve); (3, Day03.solve) 
          (4, Day04.solve); (5, Day05.solve); (6, Day06.solve)
        ] |> Map.ofList

    match argv |> List.ofArray with
    | [] -> Map.iter (fun _ solve -> solve()) solutions
    | days ->
        days |> List.iter (fun day ->
            match Int32.TryParse day with
            | true, day when day > 0 && day < 26 -> 
                match Map.tryFind day solutions with
                | Some solve -> solve()
                | _ -> printfn "Day %d hasn't been implemented yet" day
            | _ -> printfn "Invalid argument, day must be an integer between 1-25")
        

    0 // return an integer exit code
