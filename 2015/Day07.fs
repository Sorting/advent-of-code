namespace Year2015

module Day07 =
    open Utilities

    type Instruction =
        | AssignByValue of string * int
        | AssignByWire of string * string
        | And of string * string * string
        | AndByInt of int  * string * string
        | Or of string * string * string
        | LeftShift of string * int * string
        | RightShift of string * int * string
        | Not of string * string
        | Unknown of string

    let parseInstruction = 
        function
        | Regex @"^(\d+) -> ([a-z]+)$" [value; wire] ->
            AssignByValue(wire, int value)
        | Regex @"^([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            AssignByWire(wire, assignWire)
        | Regex @"^([a-z]+) AND ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            And(leftWire, rightWire, assignWire)
        | Regex @"^(\d+) AND ([a-z]+) -> ([a-z]+)$" [value; wire; assignWire] ->
            AndByInt(int value, wire, assignWire)
        | Regex @"^([a-z]+) OR ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            Or(leftWire, rightWire, assignWire)
        | Regex @"^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            LeftShift(wire, int value, assignWire)
        | Regex @"^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            RightShift(wire, int value, assignWire)
        | Regex @"^NOT ([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            Not(wire, assignWire)
        | x -> Unknown x

    let wireExist wire circuit =
        match Map.tryFind wire circuit with
        | Some _ -> true
        | _ -> false

    type InstructionResult =
        | Ok of Map<string, int>
        | Failure
        | MissingWire

    let invokeInstruction circuit =
        function
        | AssignByValue(wire, value) -> 
            Ok (circuit |> Map.add wire value)
        | AssignByWire(wire, assignWire) ->
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire (Map.find wire circuit))
            else MissingWire
        | And(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find leftWire circuit) &&& (Map.find rightWire circuit)))
            else MissingWire
        | AndByInt(value, wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire (value &&& ((Map.find wire circuit))))
            else MissingWire
        | Or(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find leftWire circuit) ||| (Map.find rightWire circuit)))
            else MissingWire
        | LeftShift(wire, value, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire ((Map.find wire circuit) <<< value))
            else MissingWire
        | RightShift(wire, value, assignWire) -> 
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find wire circuit) >>> value))
            else MissingWire
        | Not(wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok(circuit |> Map.add assignWire (65536 + (~~~(Map.find wire circuit))))
            else MissingWire
        | Unknown _ -> 
            Failure

    let rec invokeInstructions circuit =
        function
        | [] -> circuit
        | head::tail ->
            match invokeInstruction circuit head with
            | Ok circuit    -> invokeInstructions circuit tail
            | MissingWire   -> invokeInstructions circuit (tail @ [head])
            | Failure       -> invokeInstructions circuit tail

    let getInstructions = 
        getMany 2015 7 (string)   
        |> List.ofSeq
        |> List.map parseInstruction

    let overrideWireByValue wire value instructions =
        (instructions |> List.filter (function (AssignByValue(assignWire, _)) -> assignWire <> wire | _ -> true)) 
            @ [ AssignByValue(wire, value) ]

    let getCircuit instructions = 
        instructions
        |> invokeInstructions Map.empty

    let part1() = 
        getInstructions
        |> getCircuit
        |> Map.find "a"
        
    let part2() = 
        getInstructions 
        |> overrideWireByValue "b" (part1()) 
        |> getCircuit 
        |> Map.find "a"

    let solve() = printDay 2015 7 part1 part2