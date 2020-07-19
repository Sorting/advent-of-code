namespace Year2019

module Day02 =
    open Utilities    

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory() = getSingle 2019 2 parser

    type Operation =
            | Add of int * int * int
            | Multiply of int * int * int            
            | Halt
            | UnknownOpCode   

    let instructions memory =
        let rec loop operations = function
        | opCode::x::y::z::xs ->
            let operation =
                match opCode with
                | 1  -> Add (x, y, z)
                | 2  -> Multiply (x, y, z)                
                | 99 -> Halt
                | _  -> UnknownOpCode
            loop (operations @ [operation]) xs                
        | _ -> operations
        loop [] (memory |> List.ofArray)    

    let rec executeInstructions (memory: int array) = 
       function
       | x::xs ->
            match x with
            | Add (firstAddress, secondAddress, resultAddress) ->
                memory.[resultAddress] <- memory.[firstAddress] + memory.[secondAddress]
                executeInstructions memory xs
            | Multiply (firstAddress, secondAddress, resultAddress) ->
                memory.[resultAddress] <- memory.[firstAddress] * memory.[secondAddress]
                executeInstructions memory xs           
            | Halt -> memory
            | UnknownOpCode -> failwith "Something went wrong"
        | _ -> memory     
    
    let part1() =        
        let memory = getMemory()
        memory.[1] <- 12
        memory.[2] <- 2
        Array.head (executeInstructions memory (instructions memory))

    let part2() = 
        let rec loop noun =
            let result = 
                [0..99]
                |> List.map (fun verb -> 
                    let memory = getMemory()
                    memory.[1] <- noun
                    memory.[2] <- verb
                    verb, Array.head (executeInstructions memory (instructions memory)))
                |> List.tryFind (snd >> (=) 19690720)
            match noun, result with
            | n, _ when n > 99 -> failwith "Could not find the chosen ones"            
            | _, Some (verb, _) -> 100 * noun + verb
            | _ -> loop (noun + 1)
        loop 0

    let solve() = printDay 2019 2 part1 part2