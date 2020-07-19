namespace Year2019

module Day05 =
    open Utilities    

    let parser (input: string) = input.Split(',')
    let getMemory() = getSingle 2019 5 parser

    type Parameter =
        | Position of int
        | Immediate of int       

    let parameterToInt = function Position x -> x | Immediate x -> x 

    type Instruction =
        | Add of Parameter * Parameter * Parameter 
        | Multiply of Parameter * Parameter * Parameter
        | Input of int
        | Output of int
        | JumpIfTrue of Parameter * Parameter
        | JumpIfFalse of Parameter * Parameter
        | LessThan of Parameter * Parameter * Parameter
        | Equals of Parameter * Parameter * Parameter
        | Halt
        | UnknownOpCode

    let getParameter (parameterModes: string[]) idx value =
        if parameterModes.Length - 1 < idx 
        then Position (int value)
        else
            if int parameterModes.[idx] = 1 
            then Immediate value
            else Position value
    
    let getValue (memory: int[]) = function Position x -> memory.[x] | Immediate x -> x    

    let executeInstruction (memory: int[]) output input instructionPointer =
        function  
            | Add (p1, p2, storeAddress) ->                
                memory.[parameterToInt storeAddress] <- getValue memory p1 + getValue memory p2
                output, input, instructionPointer + 4
            | Multiply (p1, p2, storeAddress) ->                
                memory.[parameterToInt storeAddress] <- getValue memory p1 * getValue memory p2
                output, input, instructionPointer + 4
            | Input address ->
                memory.[address] <- input
                output, input, instructionPointer + 2
            | Output address -> 
                printfn "%d" memory.[address]
                output @ [int memory.[address]], input, instructionPointer + 2
            | JumpIfTrue (p1, p2) -> 
                output, input, 
                    if getValue memory p1 <> 0 
                    then getValue memory p2 
                    else instructionPointer + 3
            | JumpIfFalse (p1, p2) ->
                output, input, 
                    if getValue memory p1 = 0 
                    then getValue memory p2 
                    else instructionPointer + 3
            | LessThan (p1, p2, storeAddress) ->
                memory.[parameterToInt storeAddress] <- 
                    if getValue memory p1 < getValue memory p2 
                    then 1 
                    else 0
                output, input, instructionPointer + 4
            | Equals (p1, p2, storeAddress) ->
                memory.[parameterToInt storeAddress] <- 
                    if getValue memory p1 = getValue memory p2 
                    then 1 
                    else 0
                output, input, instructionPointer + 4
            | Halt -> output, input, instructionPointer + 1
            | UnknownOpCode -> failwith "Something went wrong"        

    let executeInstructions (memory: int[]) input =
        let rec loop instructionPointer output input =
            if instructionPointer >= memory.Length 
            then memory, output
            else
                let opCode, parameterModes =
                    match string memory.[instructionPointer] with
                    | Regex @"^([0-1]{1,10})(01|02|03|04|05|06|07|08|99)$" [parameterModes; opCode] ->                                        
                        int opCode, parameterModes.ToCharArray() |> Array.rev |> Array.map string
                    | Regex @"^(1|2|3|4|5|6|7|8|99)$" [opCode] ->
                        int opCode, Array.empty
                    | _ -> 999, Array.empty
                
                let instruction =
                    match int opCode with
                    | 1  -> Add         ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )
                    | 2  -> Multiply    ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )
                    | 3  -> Input       ( int memory.[instructionPointer + 1] )
                    | 4  -> Output      ( int memory.[instructionPointer + 1] )
                    | 5  -> JumpIfTrue  ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2] )
                    | 6  -> JumpIfFalse ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2] )                                    
                    | 7  -> LessThan    ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )                                    
                    | 8  -> Equals      ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )                                    
                    | 99 -> Halt
                    | _  -> UnknownOpCode
                    
                match instruction with
                | Halt -> memory, output
                | UnknownOpCode -> loop (instructionPointer + 1) output input
                | _ ->                     
                    let output', input', instructionPointer' = executeInstruction memory output input instructionPointer instruction
                    loop instructionPointer' output' input'
                
        loop 0 [] input
    
    let part1() =        
        let memory = getMemory() |> Array.map int        
        let _, output = executeInstructions memory 1
        output |> List.tryLast |> function Some x -> x | None -> -1

    let part2() = 
        let memory = getMemory() |> Array.map int
        let _, outs = executeInstructions memory 5
        outs |> List.tryLast |> function Some x -> x | None -> -1

    let solve() = printDay 2019 5 part1 part2
    