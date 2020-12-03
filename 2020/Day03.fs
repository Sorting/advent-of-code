namespace Year2020

module Day03 =
    open Utilities
    open AdventOfCode

    type Square = 
        | Open
        | Tree

    type WalkState = {
        TreeCount: int
        OpenCount: int
    }

    let parser (lines: string list) =
        let mutable map = Array2D.create (lines.[0].Length) (Seq.length lines) Open
        lines
        |> Seq.iteri (fun y s -> 
            s.ToCharArray()
            |> Seq.iteri (fun x c -> 
                match c with
                | '.' -> map.[x, y] <- Open
                | _ -> map.[x, y] <- Tree))
        map

    let map = (getMany 2020 3 string) |> List.ofSeq |> parser

    let rec walk state pos slope map = 
        let x, y = pos
        let slopeX, slopeY = slope

        if y >= Array2D.length2 map 
        then state
        else
            let x = if x >= Array2D.length1 map then x % Array2D.length1 map else x
            let state' = 
                match map.[x, y] with
                | Tree -> { state with TreeCount = state.TreeCount + 1 }
                | _ -> { state with OpenCount = state.OpenCount + 1 }
            walk state' (x + slopeX, y + slopeY) slope map
    
    let initState = { TreeCount = 0; OpenCount = 0 }
    
    let part1() = 
        let state = walk initState (0, 0) (3, 1) map
        state.TreeCount

    let part2() =
        [ 1, 1
          3, 1
          5, 1
          7, 1
          1, 2]
        |> List.map (fun slope -> (walk initState (0, 0) slope map) |> fun state -> state.TreeCount |> int64)
        |> List.reduce (( * ))

    let solve () = printDay 2020 2 part1 part2
