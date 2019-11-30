namespace Year2015

module Day05 =
    open Utilities

    let strings = getMany 2015 5 (string)

    let containsVowels (string': string) =
        let vowels = set ['a'; 'e'; 'i'; 'o'; 'u']
        string'.ToCharArray()
        |> Array.fold (fun sum c -> if Set.contains c vowels then sum + 1 else sum) 0 > 2

    let containsDuplicates (string': string) =
        string'.ToCharArray()
        |> Array.pairwise
        |> Array.filter (fun (a, b) -> a = b)
        |> Array.length > 0

    let excludesBadWords (string': string) =
        ["ab"; "cd"; "pq"; "xy"] |> List.forall (string'.Contains >> not)

    let containsPairDuplicate (string': string) =
        string'.ToCharArray()
        |> Array.pairwise
        |> Array.pairwise        
        |> Array.countBy (id)
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.length > 0
        

    let containsRepeats (string': string) =
        string'.ToCharArray()
        |> Array.windowed 3
        |> Array.filter (function [|a; _; c|] -> a = c | _ -> false)
        |> Array.length > 0

    let part1() = 
        strings 
        |> Seq.filter (fun s -> containsDuplicates s && containsVowels s && excludesBadWords s)
        |> Seq.length
    
    let part2() =
        strings 
        |> Seq.filter (fun s -> containsPairDuplicate s && containsRepeats s)
        |> Seq.length
    
    let solve() = printDay 2015 5 part1 part2