namespace Year2019

module IntcodeComputer =
    open Utilities

    type Parameter =
        | Position of int64
        | Immediate of int64
        | Relative of address: int64 * relativeBasePointer: int64

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
        | Input of Parameter
        | Output of Parameter
        | JumpIfTrue of Parameter * Parameter
        | JumpIfFalse of Parameter * Parameter
        | LessThan of Parameter * Parameter * Parameter
        | Equals of Parameter * Parameter * Parameter
        | RelativeBaseOffset of Parameter
        | Halt
        | UnknownOpCode of int

    type ComputerState =
        { Pointer: int64
          Memory: Map<int64, int64> }

    type InstructionResult =
        | Ok of ExecutionState
        | Failure of string

    and ExecutionState =
        { Computers: Map<Amplifier, ComputerState>
          OutputBuffer: (Amplifier * int64) list
          InputBuffer: Map<Amplifier, int64 list>
          RelativeBasePointer: int64
          Pointer: int64
          Amplifier: Amplifier
          ExecutionMode: ExecutionMode }
      
    let parser (input: string) =
        input.Split(',')
        |> Array.mapi (fun i x -> int64 i, int64 x)
        |> Map.ofArray

    let opCodeWithParamsPattern = @"^([0-2]{1,10})(01|02|03|04|05|06|07|08|09|99)$"

    let validOpCodeWithParamsPattern = function
        | Regex @"^([0-2]{1,10})(01|02|03|04|05|06|07|08|09|99)$" _-> true
        | _ -> false

    let shiftAmplifier =
        function
        | A -> B
        | B -> C
        | C -> D
        | D -> E
        | E -> A

    let getAddress =
        function
        | Position x -> x        
        | Relative (address, relativeBasePointer) -> relativeBasePointer + address
        | Immediate x -> failwith "Params that we write to can never be in Immediate mode"

    let getParam (paramModes: string []) idx value relativeBasePointer =
        if paramModes.Length - 1 < idx 
        then Position(value)
        else 
            match (Array.get paramModes idx |> int) with
            | 0 -> Position value
            | 1 -> Immediate value
            | 2 -> Relative (value, relativeBasePointer)
            | x -> failwithf "Parameter mode %d is not supported" x
        

    let getValueOrDefault computerState x =
        match Map.tryFind x computerState.Memory with
            | Some value -> computerState, value
            | None -> { computerState with Memory = computerState.Memory |> Map.add x (int64 0) }, (int64 0)

    let getValue computerState =
        function
        | Position x -> getValueOrDefault computerState x
        | Immediate x -> computerState, x
        | Relative (x, relativeBasePointer) -> getValueOrDefault computerState (relativeBasePointer + x)

    let executeInstruction executionState instruction =
        let computerState =
            Map.find executionState.Amplifier executionState.Computers

        match instruction with
        | Add (p1, p2, address) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2                        

            let computerState' =
                { computerState with
                      Memory = Map.add (getAddress address) (a + b) computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (int64 4) }
        | Multiply (p1, p2, address) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2            

            let computerState' =
                { computerState with
                      Memory = Map.add (getAddress address) (a * b) computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (int64 4) }
        | Input p ->
            let computerState, value = getValue computerState p
            let x, xs, success =
                match Map.tryFind executionState.Amplifier executionState.InputBuffer with
                | Some list ->
                    match list with
                    | [] -> int64 0, [], false
                    | x' :: xs' -> x', xs', true
                | _ -> int64 0, [], false

            if not success then
                Failure(sprintf "No input available for amplifier: %A" executionState.Amplifier)
            else
                let computerState' =
                    { computerState with
                          Memory = Map.add value x computerState.Memory }

                Ok
                    { executionState with
                          Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                          InputBuffer = Map.add executionState.Amplifier xs executionState.InputBuffer
                          Pointer = executionState.Pointer + (int64 2) }
        | Output p ->
            let computerState, value = getValue computerState p            
            printfn "%A" value
            match executionState.ExecutionMode with
            | Normal ->
                Ok
                    { executionState with
                          Computers = Map.add executionState.Amplifier computerState executionState.Computers
                          OutputBuffer =
                              executionState.OutputBuffer
                              @ [ (executionState.Amplifier, value) ]
                          Pointer = executionState.Pointer + (int64 2) }
            | FeedbackLoop ->
                let nextComputerAmplifier = shiftAmplifier executionState.Amplifier

                let arguments =
                    match Map.tryFind nextComputerAmplifier executionState.InputBuffer with
                    | Some list -> list
                    | None -> []

                Ok
                    { executionState with
                          Computers = Map.add executionState.Amplifier computerState executionState.Computers
                          OutputBuffer =
                              executionState.OutputBuffer
                              @ [ (executionState.Amplifier, value) ]
                          InputBuffer = Map.add nextComputerAmplifier (arguments @ [ value ]) executionState.InputBuffer
                          Pointer = executionState.Pointer + (int64 2)
                          Amplifier = nextComputerAmplifier }
        | JumpIfTrue (p1, p2) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2            
            Ok
                { executionState with 
                      Computers = Map.add executionState.Amplifier computerState executionState.Computers                     
                      Pointer =
                          if a <> (int64 0)
                          then b
                          else executionState.Pointer + (int64 3) }
        | JumpIfFalse (p1, p2) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2
            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState executionState.Computers                     
                      Pointer =
                          if a = (int64 0)
                          then b
                          else executionState.Pointer + (int64 3) }
        | LessThan (p1, p2, address) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2

            let value =
                if a < b
                then int64 1
                else int64 0

            let computerState' =
                { computerState with
                      Memory = Map.add (getAddress address) value computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (int64 4) }
        | Equals (p1, p2, address) ->
            let computerState, a = getValue computerState p1            
            let computerState, b = getValue computerState p2

            let value =
                if a = b
                then int64 1
                else int64 0

            let computerState' =
                { computerState with
                      Memory = Map.add (getAddress address) value computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (int64 4) }
        | RelativeBaseOffset p ->
            let computerState, value = getValue computerState p
            Ok { executionState with
                    Computers = Map.add executionState.Amplifier computerState executionState.Computers
                    RelativeBasePointer = executionState.RelativeBasePointer + value
                    Pointer = executionState.Pointer + (int64 2) }
        | Halt ->
            Ok
                { executionState with
                      Pointer = executionState.Pointer + (int64 1) }
        | UnknownOpCode x -> Failure(sprintf "Unknown upcode %d" x)

    let executeInstructions computers inputBuffer amplifier executionMode relativeBasePointer =
        let rec aux (computers: Map<Amplifier, ComputerState>) pointer outputBuffer inputBuffer currentAmplifier relativeBasePointer =
            let computerState =
                match Map.tryFind currentAmplifier computers with
                | Some state -> state
                | None -> failwithf "No dedicated memory found for amplifier: %A" currentAmplifier

            let opCode, parameterModes =
                match (string (Map.find pointer computerState.Memory)) with
                | Regex opCodeWithParamsPattern [ parameterModes; opCode ] ->
                    int opCode,
                    parameterModes.ToCharArray()
                    |> Array.rev
                    |> Array.map string
                | Regex @"^(1|2|3|4|5|6|7|8|9|99)$" [ opCode ] -> int opCode, Array.empty
                | _ -> -1, Array.empty

            let instruction =
                match int opCode with
                | 1 ->
                    Add
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 2 (Map.find (pointer + (int64 3)) computerState.Memory) relativeBasePointer)
                | 2 ->
                    Multiply
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 2 (Map.find (pointer + (int64 3)) computerState.Memory) relativeBasePointer)
                | 3 -> Input(getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer)
                | 4 -> Output(getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer)
                | 5 ->
                    JumpIfTrue
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer)
                | 6 ->
                    JumpIfFalse
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer)
                | 7 ->
                    LessThan
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 2 (Map.find (pointer + (int64 3)) computerState.Memory) relativeBasePointer)
                | 8 ->
                    Equals
                        (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 1 (Map.find (pointer + (int64 2)) computerState.Memory) relativeBasePointer,
                         getParam parameterModes 2 (Map.find (pointer + (int64 3)) computerState.Memory) relativeBasePointer)
                | 9 -> RelativeBaseOffset (getParam parameterModes 0 (Map.find (pointer + (int64 1)) computerState.Memory) relativeBasePointer)
                | 99 -> Halt
                | x -> UnknownOpCode x

            match instruction with
            | Halt -> computers, outputBuffer, inputBuffer, currentAmplifier
            | Output _ ->
                match executionMode with
                | Normal ->
                    let executionState =
                        match executeInstruction
                                  { Computers = computers
                                    OutputBuffer = outputBuffer                                    
                                    InputBuffer = inputBuffer
                                    Pointer = pointer
                                    Amplifier = currentAmplifier
                                    ExecutionMode = executionMode 
                                    RelativeBasePointer = relativeBasePointer } instruction with
                        | Ok state -> state
                        | Failure message -> failwith message

                    aux executionState.Computers executionState.Pointer executionState.OutputBuffer
                        executionState.InputBuffer executionState.Amplifier executionState.RelativeBasePointer
                | FeedbackLoop ->
                    let executionState =
                        match executeInstruction
                                  { Computers = computers
                                    OutputBuffer = outputBuffer                                    
                                    InputBuffer = inputBuffer
                                    Pointer = pointer
                                    Amplifier = currentAmplifier
                                    ExecutionMode = executionMode
                                    RelativeBasePointer = relativeBasePointer } instruction with
                        | Ok state -> state
                        | Failure message -> failwith message

                    let currentComputerMemory =
                        Map.find currentAmplifier executionState.Computers
                        |> fun computerState -> computerState.Memory

                    let nextComputerPointer =
                        Map.find executionState.Amplifier executionState.Computers
                        |> fun computerState -> computerState.Pointer

                    aux
                        (Map.add currentAmplifier
                             { Pointer = executionState.Pointer
                               Memory = currentComputerMemory } executionState.Computers) nextComputerPointer
                        executionState.OutputBuffer executionState.InputBuffer executionState.Amplifier executionState.RelativeBasePointer
            | UnknownOpCode x -> failwithf "Unknown Opcode %d" x
            | _ ->
                let executionState =
                    match executeInstruction
                              { Computers = computers
                                OutputBuffer = outputBuffer
                                InputBuffer = inputBuffer
                                Pointer = pointer
                                Amplifier = currentAmplifier
                                ExecutionMode = executionMode 
                                RelativeBasePointer = relativeBasePointer } instruction with
                    | Ok state -> state
                    | Failure message -> failwith message

                aux executionState.Computers executionState.Pointer executionState.OutputBuffer
                    executionState.InputBuffer executionState.Amplifier executionState.RelativeBasePointer

        aux computers (int64 0) [] inputBuffer amplifier relativeBasePointer
