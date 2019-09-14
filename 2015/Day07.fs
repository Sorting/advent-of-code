namespace Year2015

module Day07 =
    open Utilities

    type Instruction =
        | ASSIGN of string * int
        | ASSIGNByWire of string * string
        | AND of string * string * string
        | ANDByValue of int  * string * string
        | OR of string * string * string
        | LSHIFT of string * int * string
        | RSHIFT of string * int * string
        | NOT of string * string
        | UNKNOWN of string

    let parseInstruction = 
        function
        | Regex @"^(\d+) -> ([a-z]+)$" [value; wire] ->
            ASSIGN(wire, int value)
        | Regex @"^([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            ASSIGNByWire(wire, assignWire)
        | Regex @"^([a-z]+) AND ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            AND(leftWire, rightWire, assignWire)
        | Regex @"^(\d+) AND ([a-z]+) -> ([a-z]+)$" [value; wire; assignWire] ->
            ANDByValue(int value, wire, assignWire)
        | Regex @"^([a-z]+) OR ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            OR(leftWire, rightWire, assignWire)
        | Regex @"^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            LSHIFT(wire, int value, assignWire)
        | Regex @"^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            RSHIFT(wire, int value, assignWire)
        | Regex @"^NOT ([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            NOT(wire, assignWire)
        | x -> UNKNOWN x

    let wireExist wire circuit =
        match Map.tryFind wire circuit with
        | Some _ -> true
        | _ -> false

    type InstructionResult =
        | Ok of Map<string, int>
        | Failure
        | MissingWire

    let evaluteInstruction circuit =
        function
        | ASSIGN(wire, value) -> 
            Ok (circuit |> Map.add wire value)
        | ASSIGNByWire(wire, assignWire) ->
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire (Map.find wire circuit))
            else MissingWire
        | AND(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find leftWire circuit) &&& (Map.find rightWire circuit)))
            else MissingWire
        | ANDByValue(value, wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire (value &&& ((Map.find wire circuit))))
            else MissingWire
        | OR(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find leftWire circuit) ||| (Map.find rightWire circuit)))
            else MissingWire
        | LSHIFT(wire, value, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire ((Map.find wire circuit) <<< value))
            else MissingWire
        | RSHIFT(wire, value, assignWire) -> 
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire ((Map.find wire circuit) >>> value))
            else MissingWire
        | NOT(wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok(circuit |> Map.add assignWire (65536 + (~~~(Map.find wire circuit))))
            else MissingWire
        | UNKNOWN _ -> 
            Failure

    let rec evaluteInstructions circuit =
        function
        | [] -> circuit
        | head::tail ->
            match evaluteInstruction circuit head with
            | Ok circuit    -> evaluteInstructions circuit tail
            | MissingWire   -> evaluteInstructions circuit (tail @ [head])
            | Failure       -> evaluteInstructions circuit tail

    let getInstructions = 
        getMany 2015 7 (string)   
        |> List.ofSeq
        |> List.map parseInstruction

    let overrideWire wire value instructions =
        (instructions |> List.filter (function (ASSIGN(assignWire, _)) -> assignWire <> wire | _ -> true))
            @ [ ASSIGN(wire, value) ]

    let getCircuit instructions = 
        instructions
        |> evaluteInstructions Map.empty

    let part1() = 
        getInstructions
        |> getCircuit
        |> Map.find "a"

    let part2() = 
        getInstructions 
        |> overrideWire "b" (part1()) 
        |> getCircuit 
        |> Map.find "a"

    let solve() = printDay 2015 7 part1 part2