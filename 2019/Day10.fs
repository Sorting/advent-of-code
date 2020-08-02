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
        

    let stepper grid rootPosition =
        // let mutable dict = System.Collections.Generic.HashSet<int * int>()
        let rec aux position visited angles n =
            let x, y = getCoordinates position
                 
            if Set.contains position visited then
                n
            else
                let angle = calculateAngle position rootPosition
                if Set.contains angle angles then
                    n
                else
                    let visited = Set.add position visited
                    match Map.tryFind position grid with
                    | None -> n
                    | Some item ->
                        let n', angles' =
                            match item with
                            | Empty -> n, angles
                            | _ when position = rootPosition -> n, angles
                            | _ -> n + 1, Set.add angle angles

                        [ (aux (Position(x, y + 1)) visited angles' n')
                          (aux (Position(x, y - 1)) visited angles' n')
                          (aux (Position(x + 1, y)) visited angles' n')
                          (aux (Position(x - 1, y)) visited angles' n') ] 
                        |> List.max                        

        aux rootPosition (set []) (set []) 0

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
