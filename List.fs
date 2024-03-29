namespace AdventOfCode

module List =

    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    // Much faster than anything else I've tested
    let rec permutations =
        function
        | [] -> seq [ List.empty ]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)

    and insertions x =
        function
        | [] -> [ [ x ] ]
        | (y :: ys) as xs ->
            (x :: xs) :: (List.map (fun x -> y :: x) (insertions x ys))
    