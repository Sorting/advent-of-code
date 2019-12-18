namespace Year2015

module Day05 =
    open Utilities

    let strings = getMany 2015 5 (string)

    let containsVowels (s: string) =
        let vowels = set ['a'; 'e'; 'i'; 'o'; 'u']
        s.ToCharArray()
        |> Array.fold (fun sum c -> if Set.contains c vowels then sum + 1 else sum) 0 > 2

    let containsDuplicates (s: string) =
        s.ToCharArray()
        |> Array.pairwise
        |> Array.filter (fun (a, b) -> a = b)
        |> Array.length > 0

    let excludesBadWords (s: string) = ["ab"; "cd"; "pq"; "xy"] |> List.forall (s.Contains >> not)

    let containsPairDuplicate (s: string) =
        s.ToCharArray()
        |> Array.toList 
        |> List.pairwise
        |> List.map (fun (a, b) -> string a + string b)
        |> List.fold (fun (validPairs, lastPair) pair ->
            match lastPair with
            | Some lastPair when pair = lastPair -> validPairs, Some pair
            | _                                  -> pair::validPairs, Some pair
        ) ([], None)
        |> fst
        |> List.countBy (id)            
        |> List.exists (fun (_, count) -> count > 1)

    let containsRepeats (s: string) =
        s.ToCharArray()
        |> Array.windowed 3
        |> Array.exists (function [|a; _; c|] -> a = c | _ -> false)

    let part1() = 
        strings 
        |> Seq.filter (containsDuplicates <&&> containsVowels <&&> excludesBadWords)
        |> Seq.length
    
    let part2() =
        strings 
        |> Seq.filter (containsRepeats <&&> containsPairDuplicate)
        |> Seq.length
    
    let solve() = printDay 2015 5 part1 part2