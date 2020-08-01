namespace Year2019

module Day09 =
    open Utilities

    let getMemory () = getSingle 2019 9 IntcodeComputer.parser

    let part1 () =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        let rec aux computers output =
            if not (IntcodeComputer.validOpCodeWithParamsPattern (string output)) then
                output
            else
                (IntcodeComputer.executeInstructions computers
                     (Map.ofList [ (IntcodeComputer.Amplifier.A, [ output ]) ]) IntcodeComputer.Amplifier.A
                     IntcodeComputer.ExecutionMode.Normal (int64 0))
                |> fun (computers, outputBuffer, inputBuffer, _) ->
                    aux computers
                        (outputBuffer
                        //  |> List.take (outputBuffer.Length - 1)
                         |> List.last
                         |> snd)

        IntcodeComputer.executeInstructions computers (Map.ofList [ (IntcodeComputer.Amplifier.A, [ (int64 1) ]) ])
            IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal (int64 0)
        |> fun (computers, outputBuffer, _, _) ->
            aux computers
                (outputBuffer
                //  |> List.take (outputBuffer.Length - 1)
                 |> List.last
                 |> snd)


    let part2 () = 0

    let solve () = printDay 2019 9 part1 part2
