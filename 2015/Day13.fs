namespace Year2015

module Day13 =
    open Utilities

    type Expression =
        | Expression of string * string * int 
        | Unknown

    type Seating = { Name : string; Score : int }

    let parseExpression (expression: string) =
        match expression.Split(' ') with
        | [| left; _; op; value; _; _; _; _; _; _; right |] ->
            if op = "gain"
            then Expression(left, right, int value)
            else Expression(left, right, -(int value))
        | _ -> Unknown

    let input = getMany 2015 13 (string) 

    let addOrReplace left right score graph =
        match Map.tryFind left graph with
        | Some x -> 
            graph
            |> Map.remove left
            |> Map.add left (x |> Map.add right score)
        | _ ->
            graph 
            |> Map.add left (Map.empty |> Map.add right score)

    let findMostOptimalSeating (graph : Map<string, Map<string, int>>) =
        0

    let part1() = 
        input 
        |> Seq.map parseExpression 
        |> Seq.fold 
            (fun graph expression -> 
                match expression with
                | Expression(left, right, score) -> graph |> addOrReplace left right score
                | _ -> graph) Map.empty<string, Map<string, int>>
        |> findMostOptimalSeating
        
    let part2() = 2 

    let solve() = printDay 2015 13 part1 part2