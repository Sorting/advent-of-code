
namespace Year2021

module Day02 =
    open Utilities

    type Command =
        | Forward of int
        | Down of int
        | Up of int

    let parser = function
        | Regex "forward ([0-9]+)" [steps] -> Forward (int steps)
        | Regex "down ([0-9]+)" [steps] -> Down (int steps)
        | Regex "up ([0-9]+)" [steps] -> Up (int steps)
        | _ -> failwith "Unknown command"

    let commands = getMany 2021 2 parser
    
    let part1() = 
        commands
        |> Seq.fold (fun (x, y) cmd ->
            match cmd with
            | Forward steps -> (x + steps, y)
            | Down steps -> (x, y + steps)
            | Up steps -> (x, y - steps)
        ) (0, 0)
        |> fun (x, y) -> x * y

    let part2() = 
        commands
        |> Seq.fold (fun (x, y, a) cmd ->
            match cmd with
            | Forward steps -> (x + steps, y + (a * steps), a)
            | Down steps -> (x, y, a + steps)
            | Up steps -> (x, y, a - steps)
        ) (0, 0, 0)
        |> fun (x, y, _) -> x * y

    let solve () = printDay 2021 2 part1 part2