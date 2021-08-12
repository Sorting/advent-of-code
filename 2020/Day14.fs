namespace Year2020

module Day14 =
    open Utilities
    open AdventOfCode
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
        let b = Convert.ToString(value, 2).PadLeft(36, '0').ToCharArray() 
        bitMask 
        |> Seq.iteri (fun i x -> if x <> 'X' then b.[i] <- x)
        Convert.ToInt64(Array.map string b |> String.concat "", 2)

    let getAll arr = 
        let n = Array.length arr
        [ for i in 0..n -> 
            List.concat [ String('0', n - i).ToCharArray() |> List.ofArray
                          String('1', i).ToCharArray() |> List.ofArray ]
                        |> List.permutations
                        |> Seq.distinct ]
        |> Seq.collect id
        |> List.ofSeq
        |> List.map (fun x -> x |> List.mapi (fun i x -> arr.[i], x))
    
    let applyMaskV2 (bitMask: string) (value: int64) =
        let b = Convert.ToString(value, 2).PadLeft(36, '0').ToCharArray()
        let bits = 
             bitMask 
             |> Seq.fold (fun (idx, acc) x ->
                match x with
                | '0' ->  idx+1, acc 
                | '1' -> 
                    b.[idx] <- '1'
                    idx+1, acc 
                | 'X' ->
                    idx+1, idx :: acc
                | _ -> failwithf "%c is not an expected character in the bit mask" x
               ) (0, [])
            |> snd
            |> List.toArray
            |> getAll 
            |> List.map (fun idxs -> 
                let temp = Array.copy b
                idxs |> List.iter (fun (idx, value) -> temp.[idx] <- value)
                temp)
        if List.isEmpty bits then [ Convert.ToInt64(String b, 2) ]
        else bits |> List.map (fun bit -> Convert.ToInt64(String bit, 2))

    let rec executeInstructions computer = function
        | [] -> computer
        | x :: xs ->
            match x with
            | SetBitMask bitMask         -> executeInstructions { computer with BitMask = bitMask } xs
            | SetMemory (address, value) -> executeInstructions { computer with Memory = computer.Memory |> Map.add address (applyMask computer.BitMask value) } xs

    let rec executeInstructionsV2 computer = function
        | [] -> computer
        | x :: xs ->
            match x with
            | SetBitMask bitMask         -> executeInstructionsV2 { computer with BitMask = bitMask } xs
            | SetMemory (address, value) -> executeInstructionsV2 { computer with Memory = applyMaskV2 computer.BitMask address |> List.fold (fun memory address -> Map.add address value memory) computer.Memory } xs

    let solver f = Map.toList (f { BitMask = ""; Memory = Map.empty } instructions).Memory |> List.sumBy snd

    let part1() = solver executeInstructions
    let part2() = solver executeInstructionsV2

    let solve () = printDay 2020 14 part1 part2
    