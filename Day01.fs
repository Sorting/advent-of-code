module Day01
open Utilities

let frequencies = getMany 1 (function value when Seq.head value = '+' -> int (value.Substring(1)) | value -> int value)

let part1 = Seq.sum frequencies

let part2 =
    let list = frequencies
    let rec changeFrequencies memory initialValue =
        let frequencies = 
            list 
            |> Seq.scan (+) initialValue 
            |> Seq.tail
        match Seq.tryFind (fun frequency -> Set.contains frequency memory) frequencies with
        | Some firstDuplicate -> firstDuplicate
        | _ -> changeFrequencies (Set.union (Set.ofSeq frequencies) memory) (Seq.last frequencies)
    changeFrequencies (set []) 0

let solve() =
    printfn "### Day 01 ###"
    printfn "Part 1: %d" part1
    printfn "Part 2: %A" part2