namespace Year2019

module Day05 =
    open Utilities
    open System    

    let parser (input: string) = input.Split(',')
    let getMemory() = getSingle 2019 5 parser

    type Parameter =
        | Position of int
        | Immediate of int       

    let parameterToInt = function Position x -> x | Immediate x -> x 

    type Operation =
            | Add of Parameter list
            | Multiply of Parameter list
            | Input of int
            | Output of int
            | Halt
            | UnknownOpCode

    let getParameter (parameterModes: string list) idx (value: string) =
        if parameterModes.Length - 1 < idx 
        then Position (int value)
        else
            if int parameterModes.[idx] = 1 
            then Immediate (int value)
            else Position (int value)
        

    let instructions memory =
        let rec loop operations = function
            | x::xs ->
                match x with
                | Regex @"^([0-1]{1,10})(01|02|03|04|99)$" [parameterModes; opCode] ->                                        
                    let parameterModes' = parameterModes.ToCharArray() |> Array.rev |> Array.map string |> Array.toList
                    let operation, argsCount =
                        match int opCode with
                        | 1  -> Add ([ for i in 0 .. 2 -> getParameter parameterModes' i xs.[i] ]), 3
                        | 2  -> Multiply ([ for i in 0 .. 2 -> getParameter parameterModes' i xs.[i] ]), 3
                        | 3  -> Input (int xs.[0]), 1
                        | 4  -> Output (int xs.[0]), 1
                        | 99 -> Halt, 0
                        | _  -> UnknownOpCode, 0
                    loop (operations @ [operation]) (xs |> List.skip argsCount)
                | Regex @"^(1|2|3|4|99)$" [opCode] ->                    
                    let operation, argsCount = 
                        match int opCode with
                        | 1  -> Add [ Position (int xs.[0]); Position (int xs.[1]); Position (int xs.[2]) ], 3
                        | 2  -> Multiply [ Position (int xs.[0]); Position (int xs.[1]); Position (int xs.[2]) ], 3
                        | 3  -> Input (int xs.[0]), 1
                        | 4  -> Output (int xs.[0]), 1
                        | 99 -> Halt, 0
                        | _  -> UnknownOpCode, 0
                    loop (operations @ [operation]) (xs |> List.skip argsCount)        
                | _ -> loop operations xs
            | _ -> operations
        loop [] (memory |> List.ofArray)

    let getValue (memory: int array) = function Position x -> memory.[x] | Immediate x -> x

    let rec executeInstructions (memory: int array) (output: int list) (input: int) = 
       function
       | x::xs ->
            match x with
            | Add (parameters) ->
                let resultAddress = List.last parameters                
                let parameters' = parameters |> List.take ((List.length parameters) - 1)
                memory.[parameterToInt resultAddress] <- parameters' |> List.sumBy (getValue memory)
                executeInstructions memory output input xs
            | Multiply (parameters) ->
                let resultAddress = List.last parameters                
                let parameters' = parameters |> List.take ((List.length parameters) - 1)
                memory.[parameterToInt resultAddress] <- parameters' |> List.fold (fun acc x -> acc * (getValue memory x)) 1
                executeInstructions memory output input xs
            | Input value ->
                let input' = int (Console.ReadLine())
                memory.[value] <- input'              
                executeInstructions memory output input' xs
            | Output address -> 
                printfn "%d" memory.[address]
                executeInstructions memory (output @ [memory.[address]]) input xs
            | Halt -> memory, output
            | UnknownOpCode -> failwith "Something went wrong"
        | _ -> memory, output
    
    let part1() =        
        let memory = getMemory()        
        let mems, outs = (executeInstructions (memory |> Array.map int) [] 0 (instructions memory))
        outs |> List.tryLast |> function Some x -> x | None -> 666

    let part2() = 
        0

    let solve() = printDay 2019 5 part1 part2
    

