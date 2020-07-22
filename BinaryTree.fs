module BinaryTree

type BinaryTree<'a> = 
    | Node of value: 'a * left: BinaryTree<'a> * right: BinaryTree<'a>
    | Empty

let rec exists value = function
    | Empty -> false
    | Node(x, Empty, Empty) -> x = value        
    | Node(x, left, right) ->
        if x = value 
        then true
        else exists value left || exists value right

let rec findNode value = function
    | Empty -> None
    | Node(x, left, right) ->
        if x = value 
        then Some (Node(x, left, right))
        else 
            match findNode value left with
            | Some x -> Some x
            | None -> findNode value right

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

let rec add parentValue value = function
    | Empty -> Node(value, Empty, Empty)
    | Node(x, Empty, Empty) ->
        if x = parentValue 
        then Node(x, Node(value, Empty, Empty), Empty)
        else Node(x, Empty, Empty)
    | Node(x, left, Empty) ->
        if x = parentValue 
        then Node(x, left, Node(value, Empty, Empty))
        else Node (x, add parentValue value left, Empty)
    | Node(x, Empty, right) ->
        if x = parentValue 
        then Node(x, Node(value, Empty, Empty), right)
        else Node (x, Empty, add parentValue value right)
    | Node(x, left, right) ->
        if x = parentValue
        then failwith "Already occupied"
        else Node(x, add parentValue value left, add parentValue value right)

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
    