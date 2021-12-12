namespace Year2021

module Day01 =
    open Utilities

    let numbers = getMany 2021 1 int 
    
    let part1() = 
        numbers |> Seq.fold (fun (count, prev) x -> 
            match prev with
            | Some(p) -> (count + (if x > p then 1 else 0), Some(x))
            | None -> (count, Some(x))) (0, None) 
        |> fst

    let part2() =
        numbers 
        |> Seq.windowed 3 
        |> Seq.fold (fun (count, prev) x -> 
            let sum = Seq.sum x
            match prev with
            | Some(p) -> (count + (if sum > p then 1 else 0), Some(sum))
            | None -> count, Some(sum)) (0, None)
        |> fst

    let solve () = printDay 2021 1 part1 part2