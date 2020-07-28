module BinaryTree

type BinaryTree<'a> =
    | Node of value: 'a * left: BinaryTree<'a> * right: BinaryTree<'a>
    | Empty

let rec exists value =
    function
    | Empty -> false
    | Node (x, Empty, Empty) -> x = value
    | Node (x, left, right) -> if x = value then true else exists value left || exists value right

let rec findNode value =
    function
    | Empty -> None
    | Node (x, left, right) ->
        if x = value then
            Some(Node(x, left, right))
        else
            match findNode value left with
            | Some x -> Some x
            | None -> findNode value right

let findPath value =
    function
    | Empty -> None
    | root ->
        let rec aux depth path =
            function
            | Empty -> None
            | Node (x, left, right) ->
                if x = value then
                    Some(path, depth)
                else
                    match aux (depth + 1) (path @ [ x ]) left with
                    | Some x' -> Some x'
                    | None -> aux (depth + 1) (path @ [ x ]) right

        aux 0 [] root

let rec add parentValue value =
    function
    | Empty -> Node(value, Empty, Empty)
    | Node (x, Empty, Empty) ->
        if x = parentValue
        then Node(x, Node(value, Empty, Empty), Empty)
        else Node(x, Empty, Empty)
    | Node (x, left, Empty) ->
        if x = parentValue
        then Node(x, left, Node(value, Empty, Empty))
        else Node(x, add parentValue value left, Empty)
    | Node (x, Empty, right) ->
        if x = parentValue
        then Node(x, Node(value, Empty, Empty), right)
        else Node(x, Empty, add parentValue value right)
    | Node (x, left, right) ->
        if x = parentValue
        then failwith "Already occupied"
        else Node(x, add parentValue value left, add parentValue value right)

let findCommonRoot a b root =
    let rec aux last =
        function
        | [], [] -> Some last
        | [ _ ], [ _ ] -> Some last
        | x1 :: xs1, x2 :: xs2 -> if x1 <> x2 then Some last else aux x1 (xs1, xs2)
        | _ -> None

    let aPath =
        match findPath a root with
        | Some (path, _) -> path
        | None -> []

    let bPath =
        match findPath b root with
        | Some (path, _) -> path
        | None -> []

    findNode
        (match (aux Unchecked.defaultof<'a> (aPath, bPath)) with
         | Some x -> x
         | None -> Unchecked.defaultof<'a>) root

let countSteps a b =
    function
    | Empty -> 0
    | root ->
        let aSteps =
            match findPath a root with
            | Some (_, steps) -> steps
            | None -> 1

        let bSteps =
            match findPath b root with
            | Some (_, steps) -> steps
            | None -> 1

        (aSteps - 1) + (bSteps - 1)

let countDirectAndIndirect =
    function
    | Empty -> 0
    | Node (_, root, _) ->
        let rec count n =
            function
            | Empty -> n
            | Node (_, left, right) -> n + (count (n + 1) left) + (count (n + 1) right)

        (count 0 root) / 2
