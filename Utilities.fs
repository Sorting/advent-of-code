module Utilities
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open System.Threading
open System

let getInputPath day = sprintf "%s/input/day%s.txt" (Directory.GetCurrentDirectory()) (if day < 10 then sprintf "0%d" day else string day)
let getSingle day parser = File.ReadAllText(getInputPath day, Encoding.UTF8) |> parser
let getMany day parser = File.ReadAllLines(getInputPath day, Encoding.UTF8) |> Array.map parser |> Array.toSeq

let printDay day part1 part2 =
    Console.ForegroundColor <- ConsoleColor.Yellow
    printfn "### Day %s ###" (if day < 10 then sprintf "0%d" day else string day)
    Console.ForegroundColor <- ConsoleColor.White
    let printExecutionTime seconds = 
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "- It took %f seconds" seconds
        Console.ForegroundColor <- ConsoleColor.White
    
    let sw = Stopwatch.StartNew()

    printfn "Part 1: %s" (part1().ToString())
    printExecutionTime sw.Elapsed.TotalSeconds
    sw.Restart()
    
    printfn "Part 2: %s" (part2().ToString())
    printExecutionTime sw.Elapsed.TotalSeconds
    sw.Stop()

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let unpairwise (x) = 
    seq {
        if not(Seq.isEmpty x) then
            let (a, b) = Seq.head x
            yield a
            yield b
            yield! Seq.skip 1 x |> Seq.map (fun (_, b) -> b)
    }
