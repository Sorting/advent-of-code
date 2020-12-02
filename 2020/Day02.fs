namespace Year2020

module Day02 =
    open Utilities
    open AdventOfCode

    type PasswordPolicy = 
        | Policy of (int * int) * char * string
        | Error

    let parser (input: string) =
        match input with
        | Regex "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$" [min; max; character; password] ->
            Policy((int min, int max), character.ToCharArray() |> Array.head, password)
        | _ -> Error

    let policies = getMany 2020 2 parser

    let validateOccurrences = function
        | Policy((min, max), character, password) ->
            password.ToCharArray()
            |> Array.countBy id
            |> Map.ofArray
            |> Map.tryFind character
            |> function
                | Some count -> count >= min && count <= max
                | _ -> false
        | _ -> false

    let validatePositions = function
        | Policy((idx1, idx2), character, password) ->
            let arr = password.ToCharArray()
            let a, b = arr.[idx1-1], arr.[idx2-1]
            (a = character || b = character) && a <> b
        | _ -> false
    
    let part1() =
       policies
       |> Seq.map validateOccurrences
       |> Seq.filter ((=) true)
       |> Seq.length

    let part2() = 
       policies
       |> Seq.map validatePositions
       |> Seq.filter ((=) true)
       |> Seq.length

    let solve () = printDay 2020 2 part1 part2
