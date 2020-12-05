namespace Year2020

module Day04 =
    open Utilities
    open System.Text.RegularExpressions
    open System

    type HeightUnit =
        | Cm
        | In

    type FieldType =
        | Year of int
        | Height of int * HeightUnit
        | HairColor of string
        | EyeColor of string
        | PassportId of string
        | Invalid

    type FieldValidationResult =
        | Ok
        | Failed

    let parser input =
        Regex.Split(input, "\n\n")
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.map (fun data ->
            Regex.Split(data, "\n| ")
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
            |> Array.map (fun kv -> 
                match kv.Split(':', StringSplitOptions.RemoveEmptyEntries) with
                | [|key; value|] -> (key, value)
                | _ -> failwith "Invalid data")
            |> Map.ofArray)

    let passports = getSingle 2020 4 parser
    let requiredFields = set ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    let hasRequiredFields passport = requiredFields |> Set.forall (fun rf -> Map.containsKey rf passport)

    let tryParseYear (value: string) =
        match Int32.TryParse value with
        | true, x -> Year(x)
        | _ -> Invalid

    let validEyeColors = set ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

    let hasValidFieldValue = function
        | ("byr", value) -> 
            match tryParseYear value with
            | Year year when year >= 1920 && year <= 2002 -> Ok
            | _ -> Failed
        | ("iyr", value) -> 
            match tryParseYear value with
            | Year year when year >= 2010 && year <= 2020 -> Ok
            | _ -> Failed
        | ("eyr", value) -> 
            match tryParseYear value with
            | Year year when year >= 2020 && year <= 2030 -> Ok
            | _ -> Failed
        | ("hgt", value) ->
            match value with
            | Regex "^(\\d+)(cm|in)$" [height; unit] ->
                let heightField = 
                    match unit with
                    | "cm" -> Height(int height, Cm)
                    | "in" -> Height(int height, In)
                    | _ -> Invalid
                match heightField with
                | Height(height, Cm) when height >= 150 && height <= 193 -> Ok
                | Height(height, In) when height >= 59 && height <= 76 -> Ok
                | _ -> Failed
            | _ -> Failed
        | ("hcl", value) -> 
            match value with
            | Regex "^#([0-9a-f]{6})$" [_] -> Ok
            | _ -> Failed 
        | ("ecl", value) -> if Set.contains value validEyeColors then Ok else Failed
        | ("pid", value) ->
            match value with
            | Regex "^([0-9]{9})$" [_] -> Ok
            | _ -> Failed
        | ("cid", _) -> Ok
        | _ -> Failed

    let part1() = 
        passports
        |> Array.filter hasRequiredFields
        |> Array.length

    let part2() =
        passports
        |> Array.filter (fun fields -> hasRequiredFields fields && (Map.forall (fun key value -> hasValidFieldValue (key, value) = Ok) fields))
        |> Array.length

    let solve () = printDay 2020 4 part1 part2
