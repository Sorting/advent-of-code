namespace Year2019

module Day05 =
    open Utilities    

    let parser (input: string) = input.Split(',')
    let getMemory() = getSingle 2019 5 parser
    
    let part1() =        
        let memory = getMemory() |> Array.map int
        IntCodeComputer.executeInstructions memory [1]
        |> snd 
        |> List.last   

    let part2() = 
        let memory = getMemory() |> Array.map int
        IntCodeComputer.executeInstructions memory [5] 
        |> snd 
        |> List.last        

    let solve() = printDay 2019 5 part1 part2
    