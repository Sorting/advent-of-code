module Day03
open System.Text.RegularExpressions
open Utilities
open System.Diagnostics
open System.Threading

type Claim = { id: int; x: int; y: int; width: int; height: int }
type Inch = { claimId: int; x: int; y: int }


let claimParser line =
    let m = Regex.Match(line, @"\#(\d+) \@ (\d+),(\d+): (\d+)x(\d+)")
    { id = int m.Groups.[1].Value
      x = int m.Groups.[2].Value
      y = int m.Groups.[3].Value
      width = int m.Groups.[4].Value
      height = int m.Groups.[5].Value }

let inches =
    getMany 3 claimParser
    |> Seq.collect (fun claim ->
        seq { 
            for y in claim.y+1..claim.y+claim.height do
                yield! seq {
                    for x in claim.x+1..claim.x+claim.width ->
                        { claimId = claim.id; x = x; y = y } } })

let part1() =
    Seq.groupBy (fun inch -> inch.x, inch.y) inches
    |> Seq.filter (fun (_, x) -> Seq.length x > 1)
    |> Seq.length

let part2() =
    let claimsCount =
        Seq.groupBy (fun inch -> inch.claimId) inches
        |> Seq.map (fun (key, values) -> key, Seq.length values)
        |> Map.ofSeq

    Seq.groupBy (fun inch -> inch.x, inch.y) inches
    |> Seq.filter (fun (_, values) -> Seq.length values = 1)
    |> Seq.collect (fun (_, values) -> values)
    |> Seq.groupBy (fun inch -> inch.claimId)
    |> Seq.find (fun (claimId, value) -> Seq.length value = Map.find claimId claimsCount)
    |> fun (claimId, _) -> claimId

let solve() = printDay 3 part1 part2