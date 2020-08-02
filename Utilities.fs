module Utilities

open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open System

let getInputPath year day =
    sprintf "%s/%d/input/day%s.txt" (Directory.GetCurrentDirectory()) year
        (if day < 10 then sprintf "0%d" day else string day)

let getSingle year day parser =
    File.ReadAllText(getInputPath year day, Encoding.UTF8)
    |> parser

let getMany year day parser =
    File.ReadAllLines(getInputPath year day, Encoding.UTF8)
    |> Array.map parser
    |> Array.toSeq

let (<&&>) f g s = f s && g s

let printDay year day part1 part2 =
    Console.ForegroundColor <- ConsoleColor.Yellow
    printfn "### %d Day %s ###" year (if day < 10 then sprintf "0%d" day else string day)
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
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let manhattanDistance = fun (a, b) (c, d) -> abs (a - c) + abs (b - d)

let unpairwise (x) =
    seq {
        if not (Seq.isEmpty x) then
            let (a, b) = Seq.head x
            yield a
            yield b
            yield! Seq.skip 1 x |> Seq.map (fun (_, b) -> b)
    }
