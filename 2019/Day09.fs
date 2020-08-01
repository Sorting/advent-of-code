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

        IntcodeComputer.executeInstructions computers (Map.ofList [ (IntcodeComputer.Amplifier.A, [ (int64 1) ]) ])
            IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal (int64 0)
        |> fun (_, outputBuffer, _, _) ->
            (outputBuffer            
             |> List.last
             |> snd)


    let part2 () =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        IntcodeComputer.executeInstructions computers (Map.ofList [ (IntcodeComputer.Amplifier.A, [ (int64 2) ]) ])
            IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal (int64 0)
        |> fun (_, outputBuffer, _, _) ->
            (outputBuffer            
             |> List.last
             |> snd)

    let solve () = printDay 2019 9 part1 part2
