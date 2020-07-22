namespace Year2019

module Day06 =    
    open Utilities

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
    
    let rec findNode node = function
        | Empty -> None
        | Node(x, left, right) ->
            if x = node 
            then Some (Node(x, left, right))
            else 
                match findNode node left with
                | Some x -> Some x
                | None -> findNode node right
    
    let rec addNode parent value = function
        | Empty -> Node(value, Empty, Empty)
        | Node(x, Empty, Empty) ->
            if x = parent 
            then Node(x, Node(value, Empty, Empty), Empty)
            else Node(x, Empty, Empty)
        | Node(x, left, Empty) ->
            if x = parent 
            then Node(x, left, Node(value, Empty, Empty))
            else Node (x, addNode parent value left, Empty)
        | Node(x, Empty, right) ->
            if x = parent 
            then Node(x, Node(value, Empty, Empty), right)
            else Node (x, Empty, addNode parent value right)
        | Node(x, left, right) ->
            if x = parent
            then failwith "Already occupied"
            else Node(x, addNode parent value left, addNode parent value right)

    let parser (s: string) =
        let [| a; b |] = s.Split(")")
        a, b      

    let findRoot = function
        | [] -> failwith "Cannot find a root with no data"
        | list ->
            let m, s = List.fold (fun (m', s') (a, b) -> m' |> Map.add a (a, b), s' |> Set.add b) (Map.ofList [], set []) list
            Map.filter (fun a _ -> not (Set.contains a s)) m 
            |> Map.toSeq 
            |> Seq.head
            |> snd
            |> fun (a, b) -> Node(a, Node(b, Empty, Empty), Empty)


    let findPath leaf = function
        | Empty -> None
        | root ->
            let rec aux depth path = function
                | Empty -> None
                | Node(v, left, right) ->
                    if v = leaf
                    then Some (path, depth)
                    else 
                        match aux (depth + 1) (path @ [v]) left with
                        | Some v -> Some v
                        | None -> aux (depth + 1) (path @ [v]) right
            aux 0 [] root
            
        
    let buildTree pairs =
        let rec aux tree = function
            | [] -> tree
            | [(a, b)] -> addNode a b tree
            | (a, b)::xs -> 
                if not (nodeExists a tree)                
                then aux tree (xs @ [(a, b)])
                else aux (tree |> addNode a b) xs
        aux (findRoot pairs) pairs

    let findCommonRoot leaf1 leaf2 root =        
        let rec aux last = function
            | [], [] -> Some last
            | [_], [_] -> Some last
            | x1::xs1, x2::xs2 -> if x1 <> x2 then Some last else aux x1 (xs1, xs2)
            | _ -> None

        let leaf1Path = match findPath leaf1 root with Some (path, _) -> path | None -> []
        let leaf2Path = match findPath leaf2 root with Some (path, _) -> path | None -> []

        findNode (match (aux "" (leaf1Path, leaf2Path)) with Some x -> x  | None -> "") root

    let countSteps leaf1 leaf2 = function
        | Empty -> 0
        | root ->
            let leaf1Steps = match findPath leaf1 root with Some (_, steps) -> steps | None -> 1
            let leaf2Steps = match findPath leaf2 root with Some (_, steps) -> steps | None -> 1
            (leaf1Steps - 1) + (leaf2Steps - 1)

    let countDirectAndIndirect = function
    | Empty -> 0
    | Node(_, root, _) -> 
        let rec count n = function
            | Empty -> n
            | Node(_, left, right) ->
                n + (count (n + 1) left) + (count (n + 1) right)
        (count 0 root) / 2

    let part1() =
        getMany 2019 6 parser
        |> Seq.toList
        |> buildTree        
        |> countDirectAndIndirect

    let part2() =
        getMany 2019 6 parser
        |> Seq.toList
        |> buildTree        
        |> findCommonRoot "YOU" "SAN"
        |> function Some x -> countSteps "YOU" "SAN" x| None -> failwith "Nothing to see here"
        

    let solve() = printDay 2019 6 part1 part2