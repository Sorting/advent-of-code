namespace Year2019

module Day07 =
    open Utilities
    open AdventOfCode

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory () = getSingle 2019 7 parser

    let executeAmplifierController (phaseSettings: int list) =
        let memory = getMemory ()

        let computers =
            [ (IntcodeComputer.A, (0, memory))
              (IntcodeComputer.B, (0, memory))
              (IntcodeComputer.C, (0, memory))
              (IntcodeComputer.D, (0, memory))
              (IntcodeComputer.E, (0, memory)) ]
            |> Map.ofList

        let getOutput x output amplifier =
            let (_, outputValue) = output
            IntcodeComputer.executeInstructions computers ([ (amplifier, [ x; outputValue ]) ] |> Map.ofList) amplifier
                IntcodeComputer.ExecutionMode.Normal
            |> snd
            |> List.last

        let rec aux output amplifier =
            function
            | [] -> output
            | x :: xs -> aux (getOutput x output amplifier) (IntcodeComputer.shiftAmplifier amplifier) xs

        aux (IntcodeComputer.Amplifier.A, 0) IntcodeComputer.Amplifier.A phaseSettings

    let executeAmplifierControllerFeedbackLoop (phaseSettings: int list) =
        let memory = getMemory ()

        let a, b, c, d, e =
            match phaseSettings with
            | [ a; b; c; d; e ] -> a, b, c, d, e
            | list -> failwithf "Expected 5 phase setting arguments, got %d" (List.length list)

        let inputBuffer =
            Map.ofList
                [ (IntcodeComputer.A, [ a; 0 ])
                  (IntcodeComputer.B, [ b ])
                  (IntcodeComputer.C, [ c ])
                  (IntcodeComputer.D, [ d ])
                  (IntcodeComputer.E, [ e ]) ]

        let computers =
            Map.ofList
                [ (IntcodeComputer.A, (0, memory))
                  (IntcodeComputer.B, (0, (Array.copy memory)))
                  (IntcodeComputer.C, (0, (Array.copy memory)))
                  (IntcodeComputer.D, (0, (Array.copy memory)))
                  (IntcodeComputer.E, (0, (Array.copy memory))) ]

        IntcodeComputer.executeInstructions computers inputBuffer IntcodeComputer.Amplifier.A
            IntcodeComputer.ExecutionMode.FeedbackLoop
        |> snd
        |> List.last

    let part1 () =
        List.permutations [ 0; 1; 2; 3; 4 ]
        |> Seq.map executeAmplifierController
        |> Seq.max
        |> snd

    let part2 () =
        List.permutations [ 5; 6; 7; 8; 9 ]
        |> Seq.map executeAmplifierControllerFeedbackLoop
        |> Seq.max
        |> snd

    let solve () = printDay 2019 7 part1 part2
