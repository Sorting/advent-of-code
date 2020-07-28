namespace Year2019

module Day06 =
    open Utilities

    let parser (s: string) =
        match s.Split(")") with
        | [| a; b |] -> a, b
        | _ -> failwithf "Couldn't parse: %s" s

    let findRoot =
        function
        | [] -> failwith "Cannot find a root with no data"
        | list ->
            let m, s =
                List.fold (fun (m', s') (a, b) -> m' |> Map.add a (a, b), s' |> Set.add b) (Map.ofList [], set []) list

            Map.filter (fun a _ -> not (Set.contains a s)) m
            |> Map.toSeq
            |> Seq.head
            |> snd
            |> fun (a, b) ->
                BinaryTree.Node(a, BinaryTree.Node(b, BinaryTree.Empty, BinaryTree.Empty), BinaryTree.Empty)

    let buildTree pairs =
        let rec aux tree =
            function
            | [] -> tree
            | [ (a, b) ] -> BinaryTree.add a b tree
            | (a, b) :: xs ->
                if not (BinaryTree.exists a tree) then
                    aux tree (xs @ [ (a, b) ])
                else
                    aux (tree |> BinaryTree.add a b) xs

        aux (findRoot pairs) pairs

    let part1 () =
        getMany 2019 6 parser
        |> Seq.toList
        |> buildTree
        |> BinaryTree.countDirectAndIndirect

    let part2 () =
        getMany 2019 6 parser
        |> Seq.toList
        |> buildTree
        |> BinaryTree.findCommonRoot "YOU" "SAN"
        |> function
        | Some x -> BinaryTree.countSteps "YOU" "SAN" x
        | None -> failwith "Nothing to see here"

    let solve () = printDay 2019 6 part1 part2
