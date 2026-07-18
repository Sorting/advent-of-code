
namespace Year2025

module Day01 =
    open Utilities

    type Rotation =
        | Right of int
        | Left of int
        

    let parser = function
        | Regex "L([0-9]+)" [distance] -> Left (int distance)
        | Regex "R([0-9]+)" [distance] -> Right (int distance)
        | _ -> failwith "Unknown command"

    let rotations = getMany 2025 1 parser

    let nextPosition (currentPosition, zeros) = function
        | Left distance -> 
            if currentPosition - distance < 0 then
                let res = 100 - (currentPosition - distance) % 100 
                if res = 0 then res, zeros + 1 else res, zeros
            else 
                let res = currentPosition - distance
                if res = 0 then res, zeros + 1 else res, zeros
        | Right distance -> 
            let newPos = (currentPosition + distance) % 100
            if newPos = 0 then newPos, zeros + 1 else newPos, zeros
    
    let part1() = 
        rotations
        |> Seq.fold nextPosition (50, 0)
        |> snd
    let part2() =
        0
    let solve () = printDay 2025 1 part1 part2
