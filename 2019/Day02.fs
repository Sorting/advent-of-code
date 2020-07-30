namespace Year2019

module Day02 =
    open Utilities

    let getMemory () = getSingle 2019 2 IntcodeComputer.parser

    let part1 () =
        let memory = getMemory ()

        let computers =
            Map.ofList
                [ (IntcodeComputer.A,
                   { IntcodeComputer.ComputerState.Pointer = (bigint 0)
                     IntcodeComputer.ComputerState.Memory =
                         memory
                         |> Map.add (bigint 1) (bigint 12)
                         |> Map.add (bigint 2) (bigint 2) }) ]

        IntcodeComputer.executeInstructions computers (Map.ofList [ (IntcodeComputer.Amplifier.A, [ (bigint 1) ]) ])
            IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
        |> fun (computers, _, _) -> computers
        |> Map.find IntcodeComputer.Amplifier.A
        |> fun x -> x.Memory
        |> Map.find (bigint 0)

    let part2 () =
        let rec loop (noun: int) =
            let result =
                [ 0 .. 99 ]
                |> List.map (fun verb ->
                    let memory = getMemory ()

                    let computers =
                        Map.ofList
                            [ (IntcodeComputer.A,
                               { IntcodeComputer.ComputerState.Pointer = (bigint 0)
                                 IntcodeComputer.ComputerState.Memory =
                                     memory
                                     |> Map.add (bigint 1) (bigint noun)
                                     |> Map.add (bigint 2) (bigint verb) }) ]

                    let memory =
                        IntcodeComputer.executeInstructions computers
                            (Map.ofList [ (IntcodeComputer.Amplifier.A, [ (bigint 1) ]) ]) IntcodeComputer.Amplifier.A
                            IntcodeComputer.ExecutionMode.Normal
                        |> fun (computers, _, _) -> computers
                        |> Map.find IntcodeComputer.Amplifier.A
                        |> fun x -> x.Memory

                    verb, Map.find (bigint 0) memory)
                |> List.tryFind (snd >> (=) (bigint 19690720))

            match noun, result with
            | n, _ when n > 99 -> failwith "Couldn't find the chosen ones"
            | _, Some (verb, _) -> 100 * noun + verb
            | _ -> loop (noun + 1)

        loop 0

    let solve () = printDay 2019 2 part1 part2
