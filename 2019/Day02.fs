namespace Year2019

module Day02 =
    open Utilities

    let parser (input: string) = input.Split(',') |> Array.map int
    let getMemory () = getSingle 2019 2 parser

    let part1 () =
        let memory = getMemory ()

        let computers =
            Map.ofList [ (IntcodeComputer.A, (0, memory)) ]

        Array.set memory 1 12
        Array.set memory 2 2

        Array.head
            (IntcodeComputer.executeInstructions computers (Map.ofList [ (IntcodeComputer.Amplifier.A, [ 1 ]) ])
                 IntcodeComputer.Amplifier.A IntcodeComputer.ExecutionMode.Normal
             |> fst)

    let part2 () =
        let rec loop noun =
            let result =
                [ 0 .. 99 ]
                |> List.map (fun verb ->
                    let memory = getMemory ()

                    let computers =
                        Map.ofList [ (IntcodeComputer.A, (0, memory)) ]

                    Array.set memory 1 noun
                    Array.set memory 2 verb

                    let res, _ =
                        IntcodeComputer.executeInstructions computers
                            (Map.ofList [ (IntcodeComputer.Amplifier.A, [ 1 ]) ]) IntcodeComputer.Amplifier.A
                            IntcodeComputer.ExecutionMode.Normal

                    verb, Array.head res)
                |> List.tryFind (snd >> (=) 19690720)

            match noun, result with
            | n, _ when n > 99 -> failwith "Couldn't find the chosen ones"
            | _, Some (verb, _) -> 100 * noun + verb
            | _ -> loop (noun + 1)

        loop 0

    let solve () = printDay 2019 2 part1 part2
