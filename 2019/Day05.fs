namespace Year2019

module Day05 =
    open Utilities

    let getMemory () = getSingle 2019 5 IntcodeComputer.parser

    let part1 () =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (bigint 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        IntcodeComputer.executeInstructions computers
            ([ (IntcodeComputer.Amplifier.A, [ (bigint 1) ]) ]
             |> Map.ofList) IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
        |> fun (_, outputBuffer, _) -> outputBuffer
        |> List.last
        |> fun (_, x) -> x

    let part2 () =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (bigint 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        IntcodeComputer.executeInstructions computers
            ([ (IntcodeComputer.Amplifier.A, [ (bigint 5) ]) ]
             |> Map.ofList) IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
        |> fun (_, outputBuffer, _) -> outputBuffer
        |> List.last
        |> fun (_, x) -> x

    let solve () = printDay 2019 5 part1 part2
