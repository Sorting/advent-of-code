namespace Year2018

module Day02  =
    open Utilities

    let boxIds = getMany 2018 2 (string)

    let containsExactly duplicatesNum value =
        Seq.groupBy (id) value
        |> Seq.exists (fun (_, values) -> Seq.length values = duplicatesNum)

    let part1() =
        Seq.fold (fun (x, y) boxId ->
            ((if containsExactly 2 boxId then x + 1 else x), (if containsExactly 3 boxId then y + 1 else y))

        ) (0, 0) boxIds
        |> fun (x, y) -> x * y

    let part2() =
        boxIds 
        |> Seq.collect (fun boxId -> [ for idx in 0..boxId.Length-1 -> (idx, boxId.Remove(idx, 1)) ])
        |> Seq.groupBy (id)
        |> Seq.find (fun (_, values) -> Seq.length values = 2)
        |> fun ((_, commonLetters), _) -> commonLetters

    let solve() = printDay 2018 2 part1 part2