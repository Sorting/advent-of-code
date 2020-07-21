namespace Year2019

module Day06 =    
    open Utilities

    // type PlanetPair = PlanetPair of string * string
    type BinaryTree = 
        | Node of value: string * left: BinaryTree * right: BinaryTree
        | Empty

    let rec nodeExists value = function
        | Empty -> false
        | Node(x, Empty, Empty) -> x = value        
        | Node(x, left, right) ->
            if x = value 
            then true
            else nodeExists value left || nodeExists value right
    
    let rec findNode value = function
        | Empty -> None
        | Node(x, Empty, Empty) -> 
            if x = value
            then Some (Node(x, Empty, Empty))        
            else None
        | Node(x, left, right ) ->
            if x = value
            then Some (Node(x, left, right))
            else 
                match findNode value left with
                | Some node -> Some node
                | None -> findNode value right
    
    let rec addNode parent value = function
        | Empty -> Node(value, Empty, Empty)
        | Node(x, Empty, Empty) ->
            if x = parent 
            then Node(x, Node(value, Empty, Empty), Empty)
            else Node (x, Empty, Empty)
        | Node(x, left, Empty) ->
            if x = parent 
            then Node(x, left, Node(value, Empty, Empty))
            else Node (x, addNode parent value left,  Empty)
        | Node(x, Empty, right) ->
            if x = parent 
            then Node(x, Node(value, Empty, Empty), right)
            else Node (x, Empty, addNode parent value right)
        | _ ->
            failwith "Already occupied"

    let parser (s: string) =
        let [| a; b |] = s.Split(")")
        a, b      

    let buildTree pairs =
        let rec aux tree = function
            | [] -> tree
            | [(a, b)] -> addNode a b tree
            | (a, b)::xs -> aux (tree |> addNode a b) xs
        let a, b = List.head pairs
        let tree = addNode a b Empty
        aux tree (pairs |> List.tail)

    let countDirectAndIndirect = function
    | Empty -> 0
    | root -> 
        let rec count n = function
            | Empty -> n
            | Node(_, left, right) ->
                n + (count (n + 1) left) + (count (n + 1) right)
        count 0 root

    let part1() =
        getMany 2019 6 parser
        |> Seq.toList
        |> buildTree
        |> countDirectAndIndirect

    let part2() = 0

    let solve() = printDay 2019 6 part1 part2