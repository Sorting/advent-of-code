namespace Year2019

module Day07 =
    open Utilities
    open AdventOfCode 

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory() = getSingle 2019 7 parser

    let executeAmplifierController (phaseSettings: int list) =
        let memory = getMemory() |> Array.map int

        let computers =
            [ (IntCodeComputer.A, (0, memory))
              (IntCodeComputer.B, (0, memory))
              (IntCodeComputer.C, (0, memory))
              (IntCodeComputer.D, (0, memory))
              (IntCodeComputer.E, (0, memory)) ] |> Map.ofList

        let getOutput x output amplifier =
            let (_, outputValue) = output
            IntCodeComputer.executeInstructions computers ([(amplifier, [x; outputValue])] |> Map.ofList) amplifier IntCodeComputer.ExecutionMode.Normal              
                |> snd                
                |> List.last                

        let rec aux output amplifier = function
            | [] -> output
            | x::xs -> aux (getOutput x output amplifier) (IntCodeComputer.shiftAmplifier amplifier) xs
        
        aux (IntCodeComputer.Amplifier.A, 0) IntCodeComputer.Amplifier.A phaseSettings

    let executeAmplifierControllerFeedbackLoop (phaseSettings: int list) =
        let memory = getMemory() |> Array.map int

        let inputBuffer =
            [ (IntCodeComputer.A, [ phaseSettings.[0]; 0 ])
              (IntCodeComputer.B, [ phaseSettings.[1] ])
              (IntCodeComputer.C, [ phaseSettings.[2] ])
              (IntCodeComputer.D, [ phaseSettings.[3] ])
              (IntCodeComputer.E, [ phaseSettings.[4] ]) ] |> Map.ofList

        let computers =
            [ (IntCodeComputer.A, (0, memory))
              (IntCodeComputer.B, (0, (Array.copy memory)))
              (IntCodeComputer.C, (0, (Array.copy memory)))
              (IntCodeComputer.D, (0, (Array.copy memory)))
              (IntCodeComputer.E, (0, (Array.copy memory))) ] |> Map.ofList

        IntCodeComputer.executeInstructions computers inputBuffer IntCodeComputer.Amplifier.A IntCodeComputer.ExecutionMode.FeedbackLoop              
                |> snd
                |> List.last
    
    let part1() =
        List.permutations [0; 1; 2; 3; 4]
        |> Seq.map executeAmplifierController        
        |> Seq.max 

    let part2() =
        List.permutations [5; 6; 7; 8; 9]
        |> Seq.map executeAmplifierControllerFeedbackLoop
        |> Seq.max

    let solve() = printDay 2019 7 part1 part2
    