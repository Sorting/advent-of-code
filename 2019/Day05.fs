namespace Year2019

module Day05 =
    open Utilities

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory () = getSingle 2019 5 parser

    let part1 () =
        let memory = getMemory () |> Array.map int

        let computers =
            Map.ofList [ (IntcodeComputer.A, (0, memory)) ]

        IntcodeComputer.executeInstructions computers
            ([ (IntcodeComputer.Amplifier.A, [ 1 ]) ]
             |> Map.ofList) IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
        |> snd
        |> List.last
        |> fun (_, x) -> x

    let part2 () =
        let memory = getMemory () |> Array.map int

        let computers =
            Map.ofList [ (IntcodeComputer.A, (0, memory)) ]

        IntcodeComputer.executeInstructions computers
            ([ (IntcodeComputer.Amplifier.A, [ 5 ]) ]
             |> Map.ofList) IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
        |> snd
        |> List.last
        |> fun (_, x) -> x

    let solve () = printDay 2019 5 part1 part2
