module Utilities
open System.IO
open System.Text

let getInputPath day = sprintf "%s/input/day%s.txt" (Directory.GetCurrentDirectory()) (if day < 10 then sprintf "0%d" day else string day)
let getSingle day parser = File.ReadAllText(getInputPath day, Encoding.UTF8) |> parser
let getMany day parser = File.ReadAllLines(getInputPath day, Encoding.UTF8) |> Array.map parser |> Array.toSeq

open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
