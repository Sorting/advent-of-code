namespace Year2020

module Day03 =
    open Utilities
    open AdventOfCode

    let parser (lines: string list) =
        let map = Array2D.create (lines.[0].Length) (Seq.length lines) '.'
        lines
        |> Seq.iteri (fun y s -> 
            s.ToCharArray()
            |> Seq.iteri (fun x c -> map.[x, y] <- c)
        )
        map

    let map = (getMany 2020 3 string) |> List.ofSeq |> parser

    let rec walk count pos slope map = 
        let x, y = pos
        let sx, sy = slope

        if y >= Array2D.length2 map 
        then count
        else walk (if map.[x % Array2D.length1 map, y] = '#' then count + 1 else count) (x + sx, y + sy) slope map
    
    let part1() = walk 0 (0, 0) (3, 1) map

    let part2() =
        [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] 
        |> List.map (fun slope -> (walk 0 (0, 0) slope map) |> int64)
        |> List.reduce (( * ))

    let solve () = printDay 2020 3 part1 part2
