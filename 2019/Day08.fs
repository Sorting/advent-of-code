namespace Year2019

module Day08 =
    open Utilities

    let w, h = 25, 6

    let toEyeFriendlyChar =
        function
        | "1" -> "#"
        | "-1" -> "\n"
        | _ -> " "

    let data =
        getSingle 2019 8 string
        |> fun x -> x.ToCharArray()
        |> Array.map (string >> int)
        |> Array.toList
        |> List.chunkBySize (w * h)

    let part1 () =
        data
        |> List.map (fun x -> x, x |> List.filter ((=) 0) |> List.length)
        |> List.minBy snd
        |> fun (list, _) ->
            (list |> List.filter ((=) 1) |> List.length)
            * (list |> List.filter ((=) 2) |> List.length)

    let part2 () =
        data
        |> List.collect List.indexed
        |> List.groupBy fst
        |> List.map (snd
             >> List.filter (snd >> (<>) 2)
             >> List.map snd
             >> List.head
             >> string
             >> toEyeFriendlyChar)
        |> List.mapi (fun i s ->            
            if i % w = 0 then sprintf "\n%s" s else s)
        |> String.concat ""        

    let solve () = printDay 2019 8 part1 part2
