namespace Year2020
open System.Collections.Generic

module Day10 =
    open Utilities

    let mutable adapters = getMany 2020 10 (bigint.Parse) |> Seq.sort |> List.ofSeq |> fun xs -> bigint 0 :: xs @ [List.max xs + bigint 3]

    let rec findDiffs counter (last: bigint) = function
        | [] -> (fst counter, snd counter), fst counter * snd counter
        | x :: xs ->
            let (ones, threes) = counter
            match int (x - last) with
            | 1 -> findDiffs (ones + 1I, threes) x xs
            | 3 -> findDiffs (ones, threes + 1I) x xs
            | _ -> findDiffs counter x xs

    let dict = Dictionary<int, bigint>()

    let rec countValidArrangements count pos =
        if pos = adapters.Length-1 then 1I
        else match dict.ContainsKey pos with
                | true  -> dict.[pos]
                | _ ->
                    let count' = 
                        [ for i in pos + 1 .. (match pos + 3 with x when x < adapters.Length -> x | _ -> adapters.Length - 1) ->
                            if adapters.[i] - adapters.[pos] <= 3I 
                            then count + countValidArrangements count i
                            else 0I ] |> List.sum
                    dict.[pos] <- count'
                    count'

    let part1() = findDiffs (bigint 0, bigint 0) 0I adapters
    let part2() = countValidArrangements 0I 0

    let solve () = printDay 2020 10 part1 part2
