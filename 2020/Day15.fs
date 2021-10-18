namespace Year2020

module Day15 =
    open Utilities

    type Game =
        { Turns: Map<int, int * int> 
          LastSpoken: int Option 
          First: bool }
  
    let numbers = getSingle 2020 15 (string >> (fun x -> x.Split(','))) |> Array.map int |> Array.toList

    let rec turn turnNumber game endTurn = function
        | [] ->
            let game' =
                match game.First with
                | true -> 
                    let isFirst, last = 
                        match Map.tryFind 0 game.Turns with
                        | Some (_, x) -> false, x
                        | None -> true, 0
                    { game with LastSpoken = Some 0; Turns = Map.add 0 (last, turnNumber) game.Turns; First = isFirst }
                | _ -> 
                    let prev, last = (Map.find game.LastSpoken.Value game.Turns)
                    let age = last - prev
                    let isFirst, last = 
                        match Map.tryFind age game.Turns with
                        | Some (_, l) -> false, l
                        | None -> true, 0
                    { game with LastSpoken = Some age; Turns = Map.add age (last, turnNumber) game.Turns; First = isFirst }
            if turnNumber = endTurn then game'.LastSpoken.Value 
            else turn (turnNumber + 1) game' endTurn []
        | x :: xs ->
            turn (turnNumber + 1) { game with Turns = Map.add x (0, turnNumber) game.Turns; LastSpoken = Some x; First = not (Map.containsKey x game.Turns) } endTurn xs

    let initState = { Turns = Map.empty; LastSpoken = None; First = false }

    let part1() = turn 1 initState 2020 numbers
    let part2() = turn 1 initState 30000000 numbers

    let solve () = printDay 2020 15 part1 part2
