
namespace Year2021

module Day03 =
    open System
    open Utilities

    let parser (s: string) = Seq.ofArray (s.ToCharArray())

    let bitSeqs = getMany 2021 3 parser
    
    let part1() =
        bitSeqs
        |> Seq.head
        |> fun bits -> 
            seq { 0..(Seq.length bits)-1 } 
            |> Seq.map (fun j -> 
                bitSeqs 
                |> Seq.map (Seq.item j)
                |> Seq.countBy id
                |> fun x -> Seq.maxBy snd x |> fst |> string, Seq.minBy snd x |> fst |> string
            )
        |> Seq.fold (fun (gamma, epsilon) (g, e) -> gamma + g, epsilon + e) ("", "")
        |> fun (gamma, epsilon) -> Convert.ToInt32(gamma, 2) * Convert.ToInt32(epsilon, 2)
    
    let part2() =
        let numbers = Seq.toList bitSeqs
        let rec most n prefered f = function
            | [x] -> Convert.ToInt32(x |> Seq.map string |> String.concat "", 2)
            | list ->
                list
                |> List.map (fun x -> Seq.item n x)
                |> List.countBy id
                |> fun [(a, ac); (b, bc)] -> if ac = bc then [(prefered, ac)] else [(a, ac); (b, bc)]
                |> (f snd >> fst)
                |> fun commonBit -> List.filter (fun bit -> Seq.item n bit = commonBit) list
                |> most (n + 1) prefered f
        let ox, scr = most 0 '1' List.maxBy numbers, most 0 '0' List.minBy numbers
        ox * scr

    let solve () = printDay 2021 3 part1 part2