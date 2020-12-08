namespace Year2020

module Day08 =
    open Utilities

    type Instruction = Jmp of int | Acc of int | Nop of int 

    let parser = function
        | Regex "^(nop|acc|jmp) ((\\+|\\-)([0-9]+))$" [op; aval; aop; bval] ->
            let value = if aop = "+" then int bval else int aval
            match op with
            | "jmp" -> Jmp value
            | "acc" -> Acc value
            | _ -> Nop value
        | _ -> failwith "Unknown instruction"

    let instructions = 
        getMany 2020 8 parser  
        |> Seq.mapi (fun i instruction -> i, instruction) 
        |> Map.ofSeq

    let rec executeInstructions instructions visited pos acc =            
            match Set.contains pos visited, Map.tryFind pos instructions with
            | true, _ -> acc, false
            | _, Some instruction ->
                match instruction with
                | Jmp value -> executeInstructions instructions (Set.add pos visited) (pos + value) acc 
                | Acc value -> executeInstructions instructions (Set.add pos visited) (pos + 1) (acc + value)
                | Nop(_)    -> executeInstructions instructions (Set.add pos visited) (pos + 1) acc
            | _ -> acc, true
    
    let rec runProgram pos instructions =
        match Map.tryFind pos instructions with 
        | Some instruction ->
            let instruction = 
                match instruction with
                | Jmp value -> Nop value
                | Nop value -> Jmp value
                | x -> x
            match instruction with
            | Acc _ -> runProgram (pos + 1) instructions
            | _ ->
                match executeInstructions (Map.add pos instruction instructions) (set []) 0 0 with
                | acc, true -> acc
                | _ -> runProgram (pos + 1) instructions
        | _ -> failwith "out of bounds"
    
    let part1() = executeInstructions instructions (set []) 0 0 |> fst
    let part2() = runProgram 0 instructions

    let solve () = printDay 2020 8 part1 part2
