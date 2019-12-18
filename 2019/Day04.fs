namespace Year2019

module Day04 =
    open Utilities
    
    let parseRange (range: string) =
        range.Split('-')
        |> function [|a; b|]-> int a, int b | _ -> failwith "Something went wrong"

    let range = getSingle 2019 4 parseRange

    let rec increases prev =
        function 
        | [] -> true
        | current::xs when current >= prev -> increases current xs
        | _ -> false 

    let hasPair = function [] -> false | digits -> List.pairwise digits |> List.exists (fun (a, b) -> a = b)

    let hasNonGroupedPair = 
        function
        | [] -> false 
        | digits -> 
            List.pairwise digits
            |> List.filter (fun (a, b) -> a = b)
            |> List.countBy (id)
            |> List.exists (fun (_, count) -> count = 1)

    let toSingleDigitList x = (string x).ToCharArray() |> Array.map int |> Array.toList            

    let part1() =
        [fst range..snd range]
        |> List.map toSingleDigitList
        |> List.filter (increases 0 <&&> hasPair)
        |> List.length
    
    let part2() =
        [fst range..snd range]
        |> List.map toSingleDigitList
        |> List.filter (increases 0 <&&> hasNonGroupedPair)
        |> List.length

    let solve() = printDay 2019 4 part1 part2