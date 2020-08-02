namespace Year2017

module Day03 =
    open Utilities

    let squareNumber = getSingle 2017 3 (int)

    type Direction = Up | Right | Down | Left
    
    type State = 
        { map: Map<int * int, int>
          position: (int * int) 
          direction: Direction }

    let initState =
        { map = Map.empty<int * int, int> |> Map.add (0, 0) 1
          position = 0, 1
          direction = Up }

    let nextState state =
        let x, y = state.position
        let tryFind key = Map.tryFind key state.map

        match state.direction with
        | Right -> match tryFind (x - 1, y) with
                   | Some _ -> { state with position = x, y+1 }
                   | _      -> { state with position = x-1, y; direction = Up }
        | Up    -> match tryFind (x, y - 1) with
                   | Some _ -> { state with position = x-1, y }
                   | _      -> { state with position = x, y-1; direction = Left }
        | Left  -> match tryFind (x + 1, y) with
                   | Some _ -> { state with position = x, y-1 }
                   | _      -> { state with position = x+1, y; direction = Down}                      
        | Down  -> match tryFind (x, y + 1) with
                   | Some _ -> { state with position = x+1, y }
                   | _      -> { state with position = x, y+1; direction = Right }

    let sumByNeighbours state = 
        let x, y = state.position
        [ x, y+1; x, y-1
          x+1, y+1; x-1, y+1
          x+1, y-1; x-1, y-1
          x+1, y; x-1, y ]
        |> List.fold (fun acc (x, y) ->
            acc +
                match Map.tryFind (x, y) state.map with
                | Some value -> value
                | _ -> 0
            ) 0

    let part1() =
        seq { 2..squareNumber-1 }
        |> Seq.fold (fun state x -> nextState { state with map = state.map |> Map.add state.position x }) initState
        |> fun state -> manhattanDistance (0, 0) state.position

    let part2() =
        let rec loop n state =
            let sum = sumByNeighbours state
            if sum > n then sum
            else loop n (nextState { state with map = state.map |> Map.add state.position sum })
        loop squareNumber initState

    let solve() = printDay 2017 3 part1 part2    