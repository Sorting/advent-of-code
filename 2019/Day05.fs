namespace Year2019

module Day05 =
    open Utilities    

    let parser (input: string) = input.Split(',')
    let getMemory() = getSingle 2019 5 parser
    
    let part1() =        
        let memory = getMemory() |> Array.map int        
        let _, output = IntCodeComputer.executeInstructions memory 1
        output |> List.tryLast |> function Some x -> x | None -> -1

    let part2() = 
        let memory = getMemory() |> Array.map int
        let _, outs = IntCodeComputer.executeInstructions memory 5
        outs |> List.tryLast |> function Some x -> x | None -> -1

    let solve() = printDay 2019 5 part1 part2
    