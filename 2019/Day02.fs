namespace Year2019

module Day02 =
    open Utilities    

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory() = getSingle 2019 2 parser      
    
    let part1() =        
        let memory = getMemory()
        memory.[1] <- 12
        memory.[2] <- 2
        
        Array.head (IntCodeComputer.executeInstructions memory [1] |> fst)

    let part2() = 
        let rec loop noun =            
            let result = 
                [0..99]
                |> List.map (fun verb -> 
                    let memory = getMemory()
                    memory.[1] <- noun
                    memory.[2] <- verb

                    let res, _ = IntCodeComputer.executeInstructions memory [1]

                    verb, Array.head res)
                |> List.tryFind (snd >> (=) 19690720)
            match noun, result with
            | n, _ when n > 99 -> failwith "Could not find the chosen ones"            
            | _, Some (verb, _) -> 100 * noun + verb
            | _ -> loop (noun + 1)
        loop 0

    let solve() = printDay 2019 2 part1 part2