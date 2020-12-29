namespace Year2020

module Day14 =
    open Utilities
    open System

    type Instruction = 
        | SetBitMask of string
        | SetMemory of address: int64 * value: int64

    type Computer =
        { Memory: Map<int64, int64>
          BitMask: string }

    let parser = function
        | Regex "^mask = (.*)$" [ mask ] ->
            SetBitMask mask
        | Regex @"^mem\[([0-9]+)\] = ([0-9]+)$" [ address; value ] ->
            SetMemory (int64 address, int64 value)
        | _ -> failwith "Invalid instruction"

    let instructions = getMany 2020 14 parser |> Seq.toList

    let applyMask (bitMask: string) (value: int64) =        
        let b = Convert.ToString(value, 2).PadLeft(36, '0').ToCharArray() |> Array.rev
        Seq.rev bitMask 
        |> Seq.iteri (fun i x -> if x <> 'X' then b.[i] <- x)
        Convert.ToInt64(Array.rev b |> Array.map string |> String.concat "", 2)

    let rec executeInstructions computer = function
        | [] -> computer
        | x :: xs ->
            match x with
            | SetBitMask bitMask         -> executeInstructions { computer with BitMask = bitMask } xs
            | SetMemory (address, value) -> executeInstructions { computer with Memory = (Map.add address (applyMask computer.BitMask value) computer.Memory) } xs

    let part1() = 
        let computer = executeInstructions { BitMask = ""; Memory = Map.empty } instructions
        Map.toList computer.Memory |> List.sumBy snd

    let part2() = 0

    let solve () = printDay 2020 14 part1 part2
    