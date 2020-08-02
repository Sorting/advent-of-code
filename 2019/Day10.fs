namespace Year2019

module Day10 =
    open Utilities

    type Item =
        | Empty
        | Asteroid

    type Position = Position of int * int


    let getCoordinates =
        function
        | (Position (x, y)) -> x, y

    let calculateAngle p1 p2 =
        let (x1, y1), (x2, y2) = getCoordinates p1, getCoordinates p2
        atan2 (float y2 - float y1) (float x2 - float x1)
        * 180.
        / System.Math.PI


    let toItem =
        function
        | '.' -> Empty
        | _ -> Asteroid

    let grid =
        getMany 2019 10 (fun x -> x.ToCharArray())
        |> Seq.mapi (fun row columns -> row, Seq.mapi (fun column value -> column, toItem value) columns)
        |> Seq.collect (fun (i, x) -> Seq.map (fun (j, y) -> Position(j, i), y) x)
        |> Map.ofSeq

    let calculateAsteroidsInSight grid rootPosition =
        grid
        |> Map.remove rootPosition
        |> Map.toList
        |> List.filter (snd >> ((=) Asteroid))
        |> List.map (fun (x, _) -> calculateAngle x rootPosition)
        |> List.distinct
        |> List.length

    let part1 () =
        grid
        |> Map.toList
        |> List.filter
            (snd
             >> function
             | Asteroid -> true
             | _ -> false)
        |> List.map (fst >> calculateAsteroidsInSight grid)
        |> List.max

    let part2 () = 0

    let solve () = printDay 2019 10 part1 part2
