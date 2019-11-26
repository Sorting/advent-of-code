namespace Year2015
open System
open System.Security.Cryptography
open System.Text

module Day03 =
    open Utilities

    let checksum = getSingle 2015 3 (string)
    let md5 = MD5.Create "md5"
    let replace (oldValue: string) (newValue: string) (str: string) = str.Replace(oldValue, newValue)

    let findLowestBy count =
        Seq.initInfinite (fun idx ->
            sprintf "%s%d" checksum (idx+1)
            |> Encoding.UTF8.GetBytes
            |> md5.ComputeHash
            |> BitConverter.ToString
            |> replace "-" "", idx+1)
        |> Seq.filter (fun (x, _) -> x.StartsWith(String('0', count)))
        |> Seq.head
        |> snd         

    let part1() = findLowestBy 5
    let part2() = findLowestBy 6
    
    let solve() = printDay 2015 3 part1 part2