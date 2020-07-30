namespace Year2019

module Day07 =
    open Utilities
    open AdventOfCode

    let getMemory () = getSingle 2019 7 IntcodeComputer.parser

    let executeAmplifierController (phaseSettings: int64 list) =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.B,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.C,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.D,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.E,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        let getOutput computers x output amplifier =
            let (_, outputValue) = output

            IntcodeComputer.executeInstructions computers ([ (amplifier, [ x; outputValue ]) ] |> Map.ofList) amplifier
                IntcodeComputer.ExecutionMode.Normal
            |> fun (computers, output, _) -> computers, output |> List.last

        let rec aux computers output amplifier =
            function
            | [] -> output
            | x :: xs ->
                let computers, output = getOutput computers x output amplifier
                aux computers output (IntcodeComputer.shiftAmplifier amplifier) xs

        aux computers (IntcodeComputer.Amplifier.A, (int64 0)) IntcodeComputer.Amplifier.A phaseSettings

    let executeAmplifierControllerFeedbackLoop (phaseSettings: int64 list) =
        let memory = getMemory ()        
        let a, b, c, d, e =
            match phaseSettings with
            | [ a; b; c; d; e ] -> a, b, c, d, e
            | list -> failwithf "Expected 5 phase setting arguments, got %d" (List.length list)

        let inputBuffer =
            Map.ofList
                [ (IntcodeComputer.A, [ a; int64 0 ])
                  (IntcodeComputer.B, [ b ])
                  (IntcodeComputer.C, [ c ])
                  (IntcodeComputer.D, [ d ])
                  (IntcodeComputer.E, [ e ]) ]

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.B,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.C,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.D,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory })
                  (IntcodeComputer.E,
                   { IntcodeComputer.ComputerState.Pointer = (int64 0)
                     IntcodeComputer.ComputerState.Memory = memory }) ]

        IntcodeComputer.executeInstructions computers inputBuffer IntcodeComputer.Amplifier.A
            IntcodeComputer.ExecutionMode.FeedbackLoop
        |> fun (_, output, _) -> List.last output

    let part1 () =
        List.permutations
            [ int64 0
              int64 1
              int64 2
              int64 3
              int64 4 ]
        |> Seq.map executeAmplifierController
        |> Seq.max
        |> snd

    let part2 () =
        List.permutations
            [ int64 5
              int64 6
              int64 7
              int64 8
              int64 9 ]
        |> Seq.map executeAmplifierControllerFeedbackLoop
        |> Seq.max
        |> snd

    let solve () = printDay 2019 7 part1 part2
