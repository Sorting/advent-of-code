module Day05

open System
open Utilities
open System

let polymers = getSingle 5 (string)

let react polymers = 
   let rec loop (polymers: string) =
      let reaction = Seq.pairwise polymers |> Seq.tryFind (fun (a, b) -> abs (int a - int b) = 32)
      match reaction with
      | Some (a, b) -> loop (polymers.Replace((string a) + (string b), ""))
      | _ -> polymers
   Seq.length (loop polymers)

let removeUnit unit = polymers.Replace(string unit, "").Replace(string (Char.ToUpper unit), "")

let part1() = react polymers
let part2() = Seq.map (removeUnit >> react) ['a'..'z'] |> Seq.min

let solve() = printDay 5 part1 part2