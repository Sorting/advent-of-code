namespace Year2019

module IntCodeComputer =     
    open Utilities
    
    type Parameter =
        | Position of int
        | Immediate of int

    type Amplifier =
        | A
        | B
        | C
        | D
        | E

    type ExecutionMode = 
        | Normal 
        | FeedbackLoop

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

    let shiftAmplifier = function
        | A -> B
        | B -> C
        | C -> D
        | D -> E
        | E -> A

    let parameterToInt = function Position x -> x | Immediate x -> x     

    let getParameter (parameterModes: string[]) idx value =
        if parameterModes.Length - 1 < idx 
        then Position (int value)
        else
            if int parameterModes.[idx] = 1 
            then Immediate value
            else Position value

    let getValue (memory: int[]) = function Position x -> memory.[x] | Immediate x -> x    

    let executeInstruction (memory: int[]) output (inputBuffer: Map<Amplifier, int list>) instructionPointer amplifier executionMode = function  
        | Add (p1, p2, storeAddress) ->                
            memory.[parameterToInt storeAddress] <- getValue memory p1 + getValue memory p2
            output, inputBuffer, instructionPointer + 4, amplifier
        | Multiply (p1, p2, storeAddress) ->                
            memory.[parameterToInt storeAddress] <- getValue memory p1 * getValue memory p2
            output, inputBuffer, instructionPointer + 4, amplifier
        | Input address ->
            let x, xs = 
                match Map.tryFind amplifier inputBuffer with
                | Some list ->                        
                    match list with
                    | [] -> failwith "No input to execute"
                    | x'::xs' -> x', xs'
                | _ -> failwith "The amplifier %A doesn't have an input values"
            
            memory.[address] <- x
            
            output, Map.add amplifier xs inputBuffer, instructionPointer + 2, amplifier
        | Output address ->             
            let value = memory.[address]
            match executionMode with
            | Normal ->
                output @ [(amplifier, value)], 
                inputBuffer,
                instructionPointer + 2,
                amplifier
            | FeedbackLoop ->                
                let nextComputerAmplifier = shiftAmplifier amplifier
                let arguments = match Map.tryFind nextComputerAmplifier inputBuffer with Some list -> list | None -> []
                
                output @ [(amplifier, value)], 
                Map.add nextComputerAmplifier (arguments @ [value]) inputBuffer,  
                instructionPointer + 2,
                nextComputerAmplifier
        | JumpIfTrue (p1, p2) -> 
            output, inputBuffer, 
                if getValue memory p1 <> 0 
                then getValue memory p2 
                else instructionPointer + 3
                , amplifier
        | JumpIfFalse (p1, p2) ->
            output, inputBuffer, 
                if getValue memory p1 = 0 
                then getValue memory p2 
                else instructionPointer + 3
                , amplifier
        | LessThan (p1, p2, storeAddress) ->
            memory.[parameterToInt storeAddress] <- 
                if getValue memory p1 < getValue memory p2 
                then 1 
                else 0
            output, inputBuffer, instructionPointer + 4, amplifier
        | Equals (p1, p2, storeAddress) ->
            memory.[parameterToInt storeAddress] <- 
                if getValue memory p1 = getValue memory p2 
                then 1 
                else 0
            output, inputBuffer, instructionPointer + 4, amplifier
        | Halt -> output, inputBuffer, instructionPointer + 1, amplifier
        | UnknownOpCode -> failwith "Something went wrong"

    let executeInstructions (computers: Map<Amplifier, (int * int[])>) inputBuffer amplifier executionMode =
        let rec loop (computers: Map<Amplifier, (int * int[])>) instructionPointer outputs inputs currentAmplifier =
            let memory =
                match Map.tryFind currentAmplifier computers with
                | Some (_, x) -> x
                | None -> failwithf "No computer found for amplifier: %A" currentAmplifier
            
            if instructionPointer >= memory.Length
            then memory, outputs
            else
                let opCode, parameterModes =
                    match string memory.[instructionPointer] with
                    | Regex @"^([0-1]{1,10})(01|02|03|04|05|06|07|08|99)$" [parameterModes; opCode] ->                                        
                        int opCode, parameterModes.ToCharArray() |> Array.rev |> Array.map string
                    | Regex @"^(1|2|3|4|5|6|7|8|99)$" [opCode] ->
                        int opCode, Array.empty
                    | _ -> -1, Array.empty
                
                let instruction =
                    match int opCode with
                    | 1  -> Add         ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )
                    | 2  -> Multiply    ( getParameter parameterModes 0 memory.[instructionPointer + 1], 
                                          getParameter parameterModes 1 memory.[instructionPointer + 2], 
                                          getParameter parameterModes 2 memory.[instructionPointer + 3] )
                    | 3  -> Input       ( memory.[instructionPointer + 1] )
                    | 4  -> Output      ( memory.[instructionPointer + 1] )
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
                | Halt -> memory, outputs
                | Output _ ->
                    match executionMode with
                    | Normal ->
                        let output', inputs', instructionPointer', amplifier' = executeInstruction memory outputs inputs instructionPointer currentAmplifier executionMode instruction
                        loop computers instructionPointer' output' inputs' amplifier'
                    | FeedbackLoop ->
                        let output', inputs', instructionPointer', nextComputerAmplifier = executeInstruction memory outputs inputs instructionPointer currentAmplifier executionMode instruction
                        let (_, currentComputerMemory) = Map.find currentAmplifier computers
                        let (nextComputerInstructionPointer, _) = Map.find nextComputerAmplifier computers
                        loop (Map.add currentAmplifier (instructionPointer', currentComputerMemory) computers) nextComputerInstructionPointer output' inputs' nextComputerAmplifier
                | UnknownOpCode -> loop computers (instructionPointer + 1) outputs inputs currentAmplifier
                | _ ->                     
                    let output', inputs', instructionPointer', amplifier' = executeInstruction memory outputs inputs instructionPointer currentAmplifier executionMode instruction
                    loop computers instructionPointer' output' inputs' amplifier'              
        loop computers 0 [] inputBuffer amplifier