namespace Year2020
open System.Collections.Generic

module Day10 =
    open Utilities

    let mutable adapters = getMany 2020 10 (int64) |> Seq.sort |> List.ofSeq |> fun xs -> 0L :: xs @ [List.max xs + 3L]

    let rec findDiffs counter last = function
        | [] -> fst counter * snd counter
        | x :: xs ->
            let (ones, threes) = counter
            match int (x - last) with
            | 1 -> findDiffs (ones + 1L, threes) x xs
            | 3 -> findDiffs (ones, threes + 1L) x xs
            | _ -> findDiffs counter x xs

    let dict = Dictionary<int, int64>()

    let rec countValidArrangements count pos =
        if pos = adapters.Length-1 then 1L
        else match dict.TryGetValue pos with
                | true, value  -> value
                | _ ->
                    let count' = 
                        [ for i in pos + 1 .. (match pos + 3 with x when x < adapters.Length -> x | _ -> adapters.Length - 1) ->
                            if adapters.[i] - adapters.[pos] <= 3L
                            then count + countValidArrangements count i
                            else 0L ] |> List.sum
                    dict.[pos] <- count'
                    count'

    let part1() = findDiffs (0L, 0L) 0L adapters
    let part2() = countValidArrangements 0L 0

    let solve () = printDay 2020 10 part1 part2
