namespace Year2019

module IntcodeComputer =
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
        | UnknownOpCode of int

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
        if paramModes.Length - 1 < idx then Position(int value)
        else if (Array.get paramModes idx |> int) = 1 then Immediate value
        else Position value

    let getValue (memory: int []) =
        function
        | Position x -> Array.get memory x
        | Immediate x -> x

    let executeInstruction (memory: int []) outputBuffer inputBuffer pointer amplifier executionMode =
        function
        | Add (p1, p2, address) ->
            Array.set memory (paramToInt address) (getValue memory p1 + getValue memory p2)
            outputBuffer, inputBuffer, pointer + 4, amplifier
        | Multiply (p1, p2, address) ->
            Array.set memory (paramToInt address) (getValue memory p1 * getValue memory p2)
            outputBuffer, inputBuffer, pointer + 4, amplifier
        | Input address ->
            let x, xs =
                match Map.tryFind amplifier inputBuffer with
                | Some list ->
                    match list with
                    | [] -> failwith "No input to execute"
                    | x' :: xs' -> x', xs'
                | _ -> failwith "The amplifier %A doesn't have an input values"

            Array.set memory address x

            outputBuffer, Map.add amplifier xs inputBuffer, pointer + 2, amplifier
        | Output address ->
            let value = Array.get memory address
            match executionMode with
            | Normal -> outputBuffer @ [ (amplifier, value) ], inputBuffer, pointer + 2, amplifier
            | FeedbackLoop ->
                let nextComputerAmplifier = shiftAmplifier amplifier

                let arguments =
                    match Map.tryFind nextComputerAmplifier inputBuffer with
                    | Some list -> list
                    | None -> []

                outputBuffer @ [ (amplifier, value) ],
                Map.add nextComputerAmplifier (arguments @ [ value ]) inputBuffer,
                pointer + 2,
                nextComputerAmplifier
        | JumpIfTrue (p1, p2) ->
            outputBuffer, inputBuffer, (if getValue memory p1 <> 0 then getValue memory p2 else pointer + 3), amplifier
        | JumpIfFalse (p1, p2) ->
            outputBuffer, inputBuffer, (if getValue memory p1 = 0 then getValue memory p2 else pointer + 3), amplifier
        | LessThan (p1, p2, address) ->
            Array.set memory (paramToInt address) (if getValue memory p1 < getValue memory p2 then 1 else 0)
            outputBuffer, inputBuffer, pointer + 4, amplifier
        | Equals (p1, p2, address) ->
            Array.set memory (paramToInt address) (if getValue memory p1 = getValue memory p2 then 1 else 0)
            outputBuffer, inputBuffer, pointer + 4, amplifier
        | Halt -> outputBuffer, inputBuffer, pointer + 1, amplifier
        | UnknownOpCode x -> failwithf "Unknown upcode %d" x

    let executeInstructions computers inputBuffer amplifier executionMode =
        let rec aux (computers: Map<Amplifier, int * int []>) pointer outputBuffer inputs currentAmplifier =
            let memory =
                match Map.tryFind currentAmplifier computers with
                | Some (_, x) -> x
                | None -> failwithf "No computer found for amplifier: %A" currentAmplifier

            if pointer >= memory.Length then
                memory, outputBuffer
            else
                let opCode, parameterModes =
                    match string memory.[pointer] with
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
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)),
                             getParam parameterModes 2 (Array.get memory (pointer + 3)))
                    | 2 ->
                        Multiply
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)),
                             getParam parameterModes 2 (Array.get memory (pointer + 3)))
                    | 3 -> Input(Array.get memory (pointer + 1))
                    | 4 -> Output(Array.get memory (pointer + 1))
                    | 5 ->
                        JumpIfTrue
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)))
                    | 6 ->
                        JumpIfFalse
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)))
                    | 7 ->
                        LessThan
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)),
                             getParam parameterModes 2 (Array.get memory (pointer + 3)))
                    | 8 ->
                        Equals
                            (getParam parameterModes 0 (Array.get memory (pointer + 1)),
                             getParam parameterModes 1 (Array.get memory (pointer + 2)),
                             getParam parameterModes 2 (Array.get memory (pointer + 3)))
                    | 99 -> Halt
                    | x -> UnknownOpCode x

                match instruction with
                | Halt -> memory, outputBuffer
                | Output _ ->
                    match executionMode with
                    | Normal ->
                        let outputBuffer', inputBuffer', pointer', amplifier' =
                            executeInstruction memory outputBuffer inputs pointer currentAmplifier executionMode
                                instruction

                        aux computers pointer' outputBuffer' inputBuffer' amplifier'
                    | FeedbackLoop ->
                        let outputBuffer', inputBuffer', pointer', nextComputerAmplifier =
                            executeInstruction memory outputBuffer inputs pointer currentAmplifier executionMode
                                instruction

                        let (_, currentComputerMemory) = Map.find currentAmplifier computers
                        let (nextComputerPointer, _) = Map.find nextComputerAmplifier computers

                        aux (Map.add currentAmplifier (pointer', currentComputerMemory) computers) nextComputerPointer
                            outputBuffer' inputBuffer' nextComputerAmplifier
                | UnknownOpCode x -> failwithf "Unknown upcode %d" x
                | _ ->
                    let outputBuffer', inputBuffer', pointer', amplifier' =
                        executeInstruction memory outputBuffer inputs pointer currentAmplifier executionMode instruction

                    aux computers pointer' outputBuffer' inputBuffer' amplifier'

        aux computers 0 [] inputBuffer amplifier
