namespace Year2015

module Day07 =
    open System
    open System.Collections.Generic
    open Utilities

    type Instruction =
        | ASSIGN of string * int
        | AND of string * string * string
        | ANDInt of int  * string * string
        | OR of string * string * string
        | LSHIFT of string * int * string
        | RSHIFT of string * int * string
        | NOT of string * string
        | UNKNOWN of string

    let parseInstruction = 
        function
        | Regex @"^(\d+) -> ([a-z]+)$" [value; wire] ->
            "", ASSIGN(wire, int value)
        | Regex @"^([a-z]+) AND ([a-z]+) -> ([a-z]+)$" [firstWire; secondWire; assignWire] ->
            firstWire, AND(firstWire, secondWire, assignWire)
        | Regex @"^(\d+) AND ([a-z]+) -> ([a-z]+)$" [value; wire; assignWire] ->
            wire, ANDInt(int value, wire, assignWire)
        | Regex @"^([a-z]+) OR ([a-z]+) -> ([a-z]+)$" [firstWire; secondWire; assignWire] ->
            firstWire, OR(firstWire, secondWire, assignWire)
        | Regex @"^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            wire, LSHIFT(wire, int value, assignWire)
        | Regex @"^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            wire, RSHIFT(wire, int value, assignWire)
        | Regex @"^NOT ([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            wire, NOT(wire, assignWire)
        | x -> "", UNKNOWN x

    // let prepareOrder instructions =
    //     instructions function
    //     | AND(firstWire, secondWire, assignWire) -> 
    //         match Map.tryFind firstWire circuit, Map.tryFind secondWire circuit with
    //         | Some _, Some _ -> 
    //             instructions 
    //             |> Map.add firstWire (AND(firstWire, secondWire, assignWire))
    //             |> Map.add secondWire (AND(firstWire, secondWire, assignWire))
    //         | Some _, None ->
    //             instructions
    //             |> Map.add firstWire (AND(firstWire, secondWire, assignWire))
    //         | None, Some _ ->             
    //             instructions
    //             |> Map.add secondWire (AND(firstWire, secondWire, assignWire))
    //         | _ -> Instruction :: instructions
    //     | ANDInt (value, wire, assignWire) ->
    //         match Map.tryFind wire circuit with
    //         | Some _ -> 
    //             circuit |> Map.add wire (ANDInt(value, wire, assignWire))
    //         | None ->
    //             circuit
    //             |> Map.add firstWire (AND(firstWire, secondWire, assignWire))
    //         | None, Some _ ->             
    //             instructions
    //             |> Map.add secondWire (AND(firstWire, secondWire, assignWire))
    //         | _ -> instructions

    let rec sortInstructions deferedInstructions =
        function
        | [] -> []
        | instructions ->
            [ for instruction in instructions do
                match instruction with
                | AND(firstWire, secondWire, _) ->
                    match Map.tryFind firstWire deferedInstructions, Map.tryFind secondWire deferedInstructions with
                    | Some _, Some _ -> 
                        yield instruction
                    | Some _, None -> 
                        yield! sortInstructions (Map.add firstWire instruction deferedInstructions) instructions
                    | None, Some _ -> Map.add secondWire instruction deferedInstructions |> ignore
                    | _ -> 
                        yield! sortInstructions 
                            (deferedInstructions
                                 |> Map.add firstWire instruction 
                                 |> Map.add secondWire instruction) 
                             instructions 
                | ANDInt(_, wire, _) -> 
                    match Map.tryFind wire deferedInstructions with
                    | Some _ -> yield instruction
                    | _ -> yield! sortInstructions (Map.add wire instruction deferedInstructions) instructions 
                | OR(firstWire, secondWire, _) -> 
                    match Map.tryFind firstWire deferedInstructions, Map.tryFind secondWire deferedInstructions with
                    | Some _, Some _ -> 
                        yield instruction
                    | Some _, None -> 
                        yield! sortInstructions (Map.add firstWire instruction deferedInstructions) instructions
                    | None, Some _ -> Map.add secondWire instruction deferedInstructions |> ignore
                    | _ -> 
                        yield! sortInstructions 
                            (deferedInstructions
                                 |> Map.add firstWire instruction 
                                 |> Map.add secondWire instruction)
                                 instructions]

                         


    let invokeInstruction circuit =
        function
        | ASSIGN(wire, value) -> circuit |> Map.add wire value
        | AND(firstWire, secondWire, newWire) -> circuit |> Map.add newWire ((Map.find firstWire circuit) &&& (Map.find secondWire circuit))
        | ANDInt(value, secondWire, newWire) -> circuit |> Map.add newWire (value &&& ((Map.find secondWire circuit)))
        | OR(firstWire, secondWire, newWire) -> circuit |> Map.add newWire ((Map.find firstWire circuit) ||| (Map.find secondWire circuit))
        | LSHIFT(wire, value, newWire) -> circuit |> Map.add newWire ((Map.find wire circuit) <<< value)
        | RSHIFT(wire, value, newWire) -> circuit |> Map.add newWire ((Map.find wire circuit) >>> value)
        | NOT(wire, newWire) -> circuit |> Map.add newWire (65536 + (~~~(Map.find wire circuit)))
        | UNKNOWN x -> circuit

    let getCircuit = 
        getMany 2015 7 (string)   
        |> Seq.map parseInstruction
        |> Seq.toList
        |> sortInstructions Map.empty<string, Instruction>
        |> Map.ofList
        |> Map.fold (fun circuit wire expression -> invokeInstruction circuit expression) Map.empty
        

    let part1() = Map.find "f" getCircuit
    let part2() = 2

    let solve() = printDay 2015 7 part1 part2