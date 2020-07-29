namespace Year2019

module Day08 =
    open Utilities

    let imageWidth = 25
    let imageHeight = 6

    let data =
        getSingle 2019 8 string
        |> fun x -> x.ToCharArray()
        |> Array.map (string >> int)
        |> Array.toList
        |> List.chunkBySize (imageWidth * imageHeight)

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
        |> List.map
            (snd
             >> List.filter (snd >> (<>) 2)
             >> List.map snd
             >> List.head
             >> string)        
        |> String.concat ""         
        |> Seq.mapi (fun i c -> if i % imageWidth = 0 then sprintf "\n%c" c else string c)
        |> String.concat ""
        |> fun x -> x.Replace('1', '#').Replace("0", " ")

    let solve () = printDay 2019 8 part1 part2
