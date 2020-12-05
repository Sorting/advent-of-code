namespace Year2020

module Day04 =
    open Utilities
    open System

    let parser (input: string) =
        input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun data ->
            data.Split([| '\n'; ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun kv -> 
                match kv.Split(':', StringSplitOptions.RemoveEmptyEntries) with
                | [|key; value|] -> (key, value)
                | _ -> failwith "Invalid data")
            |> Map.ofArray)

    let passports = getSingle 2020 4 parser
    let requiredFields = set ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    let validEyeColors = set ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

    let hasValidRange (value: string) min max = 
        match Int32.TryParse value with 
        | true, x -> x >= min && x <= max
        | _ -> false
    
    let hasRequiredFields passport = Set.forall (fun rf -> Map.containsKey rf passport) requiredFields

    let hasValidFieldValue key value = 
        match (key, value) with
        | "byr", value -> hasValidRange value 1920 2002
        | "iyr", value -> hasValidRange value 2010 2020
        | "eyr", value -> hasValidRange value 2020 2030
        | "hgt", value ->
            match value with
            | Regex "^(\\d+)(cm|in)$" [height; unit] ->
                    match unit with
                    | "cm" -> hasValidRange height 150 193
                    | "in" -> hasValidRange height 59 76
                    | _ -> false
            | _ -> false
        | "hcl", value -> match value with Regex "^#([0-9a-f]{6})$" [_] -> true | _ -> false
        | "ecl", value -> Set.contains value validEyeColors
        | "pid", value -> match value with Regex "^([0-9]{9})$" [_] -> true | _ -> false
        | "cid", _ -> true
        | _ -> false

    let part1() = 
        passports
        |> Array.filter hasRequiredFields
        |> Array.length

    let part2() =
        passports
        |> Array.filter (hasRequiredFields <&&> Map.forall hasValidFieldValue)
        |> Array.length

    let solve () = printDay 2020 4 part1 part2
