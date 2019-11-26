namespace Year2015

module Day01 =
    open Utilities

    let instructions = (getSingle 2015 1 (string)).ToCharArray() |> Array.toList

    let rec move floor = function
    | [] -> floor
    | x::xs when x = '(' -> move (floor+1) xs
    | x::xs when x = ')' -> move (floor-1) xs
    | x::_ -> failwithf "%c is an invalid character" x

    let rec moveIndex floor index instructions = 
        if floor = -1 then index
        else
            match instructions with
            | [] -> index
            | x::xs when x = '(' -> moveIndex (floor+1) (index+1) xs
            | x::xs when x = ')' -> moveIndex (floor-1)  (index+1) xs
            | x::_ -> failwithf "%c is an invalid character" x


    let part1() = move 0 instructions
    let part2() = moveIndex 0 0 instructions

    let solve() = printDay 2015 1 part1 part2