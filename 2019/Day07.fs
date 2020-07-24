namespace Year2019

module Day07 =
    open Utilities
    open AdventOfCode 

    let parser (input: string) = input.Split(',')
    let getMemory() = getSingle 2019 7 parser

    let executeAmplifierController (phaseSettings: int list) =
        let memory = getMemory() |> Array.map int

        let getOutput x output = 
            IntCodeComputer.executeInstructions memory [x; output]
                |> snd
                |> List.last

        let rec aux output = function
            | [] -> output
            | x::xs -> aux (getOutput (phaseSettings.[x]) output) xs            
        
        aux 0 [0..4]

    let executeAmplifierControllerFeedbackLoop (phaseSettings: int list) =
        let memory = (getMemory() |> Array.map int)
        let getOutput x output = 
            IntCodeComputer.executeInstructions memory [x; output]
                |> snd
                |> List.last

        let rec aux output = function
            | [] -> aux (getOutput (phaseSettings.[x]) output) [0..4]
            | x::xs -> aux (getOutput (phaseSettings.[x]) output) xs            
        
        aux 0 [0..4]    
    
    let part1() = 
        List.permutations [0; 1; 2; 3; 4]
        |> Seq.map executeAmplifierController
        |> Seq.max 

    let part2() =
        List.permutations [5; 6; 7; 8; 9]
        |> Seq.map executeAmplifierControllerFeedbackLoop
        |> Seq.max 

    let solve() = printDay 2019 7 part1 part2
    