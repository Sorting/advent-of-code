namespace Year2019

module IntcodeComputer =
    open Utilities

    type Parameter =
        | Position of bigint
        | Immediate of bigint

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
        | Input of bigint
        | Output of bigint
        | JumpIfTrue of Parameter * Parameter
        | JumpIfFalse of Parameter * Parameter
        | LessThan of Parameter * Parameter * Parameter
        | Equals of Parameter * Parameter * Parameter
        | Halt
        | UnknownOpCode of int

    type ComputerState =
        { Pointer: bigint
          Memory: Map<bigint, bigint> }

    type InstructionResult =
        | Ok of ExecutionState
        | Failure of string

    and ExecutionState =
        { Computers: Map<Amplifier, ComputerState>
          OutputBuffer: (Amplifier * bigint) list
          InputBuffer: Map<Amplifier, bigint list>
          Pointer: bigint
          Amplifier: Amplifier
          ExecutionMode: ExecutionMode }
      
    let parser (input: string) =
        input.Split(',')
        |> Array.mapi (fun i x -> bigint i, x |> bigint.Parse)
        |> Map.ofArray

    let shiftAmplifier =
        function
        | A -> B
        | B -> C
        | C -> D
        | D -> E
        | E -> A

    let paramToInt =
        function
        | Position x -> x
        | Immediate x -> x

    let getParam (paramModes: string []) idx value =
        if paramModes.Length - 1 < idx then Position(value)
        else if (Array.get paramModes idx |> int) = 1 then Immediate value
        else Position value

    let getValue (memory: Map<bigint, bigint>) =
        function
        | Position x ->
            match Map.tryFind x memory with
            | Some value -> value
            | _ -> failwithf "Memory address %A not available" x
        | Immediate x -> x

    let executeInstruction executionState instruction =
        let computerState =
            Map.find executionState.Amplifier executionState.Computers

        match instruction with
        | Add (p1, p2, address) ->
            let a = getValue computerState.Memory p1
            let b = getValue computerState.Memory p2

            let computerState' =
                { computerState with
                      Memory = Map.add (paramToInt address) (a + b) computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (bigint 4) }
        | Multiply (p1, p2, address) ->
            let a = getValue computerState.Memory p1
            let b = getValue computerState.Memory p2

            let computerState' =
                { computerState with
                      Memory = Map.add (paramToInt address) (a * b) computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (bigint 4) }
        | Input address ->
            let x, xs, success =
                match Map.tryFind executionState.Amplifier executionState.InputBuffer with
                | Some list ->
                    match list with
                    | [] -> bigint 0, [], false
                    | x' :: xs' -> x', xs', true
                | _ -> bigint 0, [], false

            if not success then
                Failure(sprintf "No input available for amplifier: %A" executionState.Amplifier)
            else
                let computerState' =
                    { computerState with
                          Memory = Map.add address x computerState.Memory }

                Ok
                    { executionState with
                          Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                          InputBuffer = Map.add executionState.Amplifier xs executionState.InputBuffer
                          Pointer = executionState.Pointer + (bigint 2) }
        | Output address ->
            let value = Map.find address computerState.Memory
            match executionState.ExecutionMode with
            | Normal ->
                Ok
                    { executionState with
                          OutputBuffer =
                              executionState.OutputBuffer
                              @ [ (executionState.Amplifier, value) ]
                          Pointer = executionState.Pointer + (bigint 2) }
            | FeedbackLoop ->
                let nextComputerAmplifier = shiftAmplifier executionState.Amplifier

                let arguments =
                    match Map.tryFind nextComputerAmplifier executionState.InputBuffer with
                    | Some list -> list
                    | None -> []

                Ok
                    { executionState with
                          OutputBuffer =
                              executionState.OutputBuffer
                              @ [ (executionState.Amplifier, value) ]
                          InputBuffer = Map.add nextComputerAmplifier (arguments @ [ value ]) executionState.InputBuffer
                          Pointer = executionState.Pointer + (bigint 2)
                          Amplifier = nextComputerAmplifier }
        | JumpIfTrue (p1, p2) ->
            Ok
                { executionState with
                      Pointer =
                          if getValue computerState.Memory p1 <> (bigint 0)
                          then getValue computerState.Memory p2
                          else executionState.Pointer + (bigint 3) }
        | JumpIfFalse (p1, p2) ->
            Ok
                { executionState with
                      Pointer =
                          if getValue computerState.Memory p1 = (bigint 0)
                          then getValue computerState.Memory p2
                          else executionState.Pointer + (bigint 3) }
        | LessThan (p1, p2, address) ->
            let value =
                if getValue computerState.Memory p1 < getValue computerState.Memory p2
                then bigint 1
                else bigint 0

            let computerState' =
                { computerState with
                      Memory = Map.add (paramToInt address) value computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (bigint 4) }
        | Equals (p1, p2, address) ->
            let value =
                if getValue computerState.Memory p1 = getValue computerState.Memory p2
                then bigint 1
                else bigint 0

            let computerState' =
                { computerState with
                      Memory = Map.add (paramToInt address) value computerState.Memory }

            Ok
                { executionState with
                      Computers = Map.add executionState.Amplifier computerState' executionState.Computers
                      Pointer = executionState.Pointer + (bigint 4) }
        | Halt ->
            Ok
                { executionState with
                      Pointer = executionState.Pointer + (bigint 1) }
        | UnknownOpCode x -> Failure(sprintf "Unknown upcode %d" x)

    let executeInstructions computers inputBuffer amplifier executionMode =
        let rec aux (computers: Map<Amplifier, ComputerState>) pointer outputBuffer inputBuffer currentAmplifier =
            let computerState =
                match Map.tryFind currentAmplifier computers with
                | Some state -> state
                | None -> failwithf "No dedicated memory found for amplifier: %A" currentAmplifier

            let opCode, parameterModes =
                match (string (Map.find pointer computerState.Memory)) with
                | Regex @"^([0-1]{1,10})(01|02|03|04|05|06|07|08|99)$" [ parameterModes; opCode ] ->
                    int opCode,
                    parameterModes.ToCharArray()
                    |> Array.rev
                    |> Array.map string
                | Regex @"^(1|2|3|4|5|6|7|8|99)$" [ opCode ] -> int opCode, Array.empty
                | _ -> -1, Array.empty

            let instruction =
                match int opCode with
                | 1 ->
                    Add
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory),
                         getParam parameterModes 2 (Map.find (pointer + (bigint 3)) computerState.Memory))
                | 2 ->
                    Multiply
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory),
                         getParam parameterModes 2 (Map.find (pointer + (bigint 3)) computerState.Memory))
                | 3 -> Input(Map.find (pointer + (bigint 1)) computerState.Memory)
                | 4 -> Output(Map.find (pointer + (bigint 1)) computerState.Memory)
                | 5 ->
                    JumpIfTrue
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory))
                | 6 ->
                    JumpIfFalse
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory))
                | 7 ->
                    LessThan
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory),
                         getParam parameterModes 2 (Map.find (pointer + (bigint 3)) computerState.Memory))
                | 8 ->
                    Equals
                        (getParam parameterModes 0 (Map.find (pointer + (bigint 1)) computerState.Memory),
                         getParam parameterModes 1 (Map.find (pointer + (bigint 2)) computerState.Memory),
                         getParam parameterModes 2 (Map.find (pointer + (bigint 3)) computerState.Memory))
                | 99 -> Halt
                | x -> UnknownOpCode x

            match instruction with
            | Halt -> computers, outputBuffer, currentAmplifier
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
                                    ExecutionMode = executionMode } instruction with
                        | Ok state -> state
                        | Failure message -> failwith message

                    aux executionState.Computers executionState.Pointer executionState.OutputBuffer
                        executionState.InputBuffer executionState.Amplifier
                | FeedbackLoop ->
                    let executionState =
                        match executeInstruction
                                  { Computers = computers
                                    OutputBuffer = outputBuffer
                                    InputBuffer = inputBuffer
                                    Pointer = pointer
                                    Amplifier = currentAmplifier
                                    ExecutionMode = executionMode } instruction with
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
                        executionState.OutputBuffer executionState.InputBuffer executionState.Amplifier
            | UnknownOpCode x -> failwithf "Unknown upcode %d" x
            | _ ->
                let executionState =
                    match executeInstruction
                              { Computers = computers
                                OutputBuffer = outputBuffer
                                InputBuffer = inputBuffer
                                Pointer = pointer
                                Amplifier = currentAmplifier
                                ExecutionMode = executionMode } instruction with
                    | Ok state -> state
                    | Failure message -> failwith message

                aux executionState.Computers executionState.Pointer executionState.OutputBuffer
                    executionState.InputBuffer executionState.Amplifier

        aux computers (bigint 0) [] inputBuffer amplifier
