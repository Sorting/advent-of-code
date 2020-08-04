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
        let convertToPositive n = if n >= 0. then n else 360. + n
        let (x1, y1), (x2, y2) = getCoordinates p1, getCoordinates p2
        ((atan2 (float y2 - float y1) (float x2 - float x1)
          * 180.
          / System.Math.PI)
         - 90.)
        |> convertToPositive

    let manhattanDistance =
        function
        | (Position (x1, y1)), (Position (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2)

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

    let vaporizeAsteroids angles =
        let rec aux n angles' =
            function
            | [] ->
                if List.isEmpty angles' then
                    printfn "Unsolveable, vaporized all asteroids before hitting 200th"
                    Position(0, 0), 0
                else
                    aux n [] angles'
            | (angle, asteroids) :: tail ->
                match asteroids with
                | [] -> aux n angles' tail
                | pos :: tail' ->
                    if n = 200 then
                        let (Position (x, y)) = pos
                        pos, ((x * 100) + y)
                    else if (List.isEmpty tail) then
                        aux (n + 1) angles' tail
                    else
                        aux (n + 1) (angles' @ [ (angle, tail') ]) tail

        aux 1 [] angles

    let part1 () =
        grid
        |> Map.toList
        |> List.filter (snd >> ((=) Asteroid))
        |> List.map (fun (position, _) -> position, calculateAsteroidsInSight grid position)
        |> List.maxBy snd

    let part2 () =
        let rootPosition = part1 () |> fst
        grid
        |> Map.remove rootPosition
        |> Map.toList
        |> List.filter (snd >> ((=) Asteroid))
        |> List.map (fun (position, _) -> position, calculateAngle position rootPosition)
        |> List.groupBy snd
        |> List.sortBy fst
        |> List.map (fun (angle, asteroids) ->
            (angle,
             asteroids
             |> List.map fst
             |> List.sortBy (fun position -> manhattanDistance (rootPosition, position))))
        |> vaporizeAsteroids


    let solve () = printDay 2019 10 part1 part2
