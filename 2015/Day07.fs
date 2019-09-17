namespace Year2015

module Day07 =
    open Utilities

    type Instruction =
        | ASSIGN of string * uint16
        | ASSIGNByWire of string * string
        | AND of string * string * string
        | ANDByValue of uint16  * string * string
        | OR of string * string * string
        | LSHIFT of string * uint16 * string
        | RSHIFT of string * uint16 * string
        | NOT of string * string
        | UNKNOWN of string

    let parseInstruction = 
        function
        | Regex @"^(\d+) -> ([a-z]+)$" [value; wire] ->
            ASSIGN(wire, uint16 value)
        | Regex @"^([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            ASSIGNByWire(wire, assignWire)
        | Regex @"^([a-z]+) AND ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            AND(leftWire, rightWire, assignWire)
        | Regex @"^(\d+) AND ([a-z]+) -> ([a-z]+)$" [value; wire; assignWire] ->
            ANDByValue(uint16 value, wire, assignWire)
        | Regex @"^([a-z]+) OR ([a-z]+) -> ([a-z]+)$" [leftWire; rightWire; assignWire] ->
            OR(leftWire, rightWire, assignWire)
        | Regex @"^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            LSHIFT(wire, uint16 value, assignWire)
        | Regex @"^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$" [wire; value; assignWire ] ->
            RSHIFT(wire, uint16 value, assignWire)
        | Regex @"^NOT ([a-z]+) -> ([a-z]+)$" [wire; assignWire] ->
            NOT(wire, assignWire)
        | x -> UNKNOWN x

    let wireExist wire circuit =
        match Map.tryFind wire circuit with
        | Some _ -> true
        | _ -> false

    type InstructionResult =
        | Ok of Map<string, uint16>
        | Failure
        | MissingWire

    let evaluteInstruction circuit =
        function
        | ASSIGN(wire, value) -> 
            Ok (circuit |> Map.add wire value)
        | ASSIGNByWire(wire, assignWire) ->
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire (circuit |> Map.find wire))
            else MissingWire
        | AND(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire (circuit |> Map.find leftWire &&& (circuit |> Map.find rightWire)))
            else MissingWire
        | ANDByValue(value, wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire (value &&& (circuit |> Map.find wire)))
            else MissingWire
        | OR(leftWire, rightWire, assignWire) -> 
            if wireExist leftWire circuit && wireExist rightWire circuit
            then Ok (circuit |> Map.add assignWire (circuit |> Map.find leftWire ||| (circuit |> Map.find rightWire)))
            else MissingWire
        | LSHIFT(wire, value, assignWire) -> 
            if wireExist wire circuit 
            then Ok (circuit |> Map.add assignWire (circuit |> Map.find wire <<< (int value)))
            else MissingWire
        | RSHIFT(wire, value, assignWire) -> 
            if wireExist wire circuit
            then Ok (circuit |> Map.add assignWire (circuit |> Map.find wire >>> (int value)))
            else MissingWire
        | NOT(wire, assignWire) -> 
            if wireExist wire circuit 
            then Ok(circuit |> Map.add assignWire (~~~(Map.find wire circuit)))
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

    let getInstructions = getMany 2015 7 parseInstruction |> Seq.toList

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