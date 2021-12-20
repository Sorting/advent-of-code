
namespace Year2021

module Day04 =
    open System.Text.RegularExpressions
    open Utilities

    type Board = Map<int * int, int * bool> 

    let parse (input: string): Board list * int list = 
        let lines = input.Split('\n') |> List.ofArray
        let numbers = List.head lines |> fun x -> x.Split(',') |> Array.map int |> Array.toList
        let rec getBoards boards currentBoard y boardNumber =
            function
            | [] -> boards, numbers
            | row :: xs ->
                if row = ""
                then 
                    if Map.isEmpty currentBoard
                    then getBoards boards currentBoard 0 boardNumber xs
                    else getBoards (boards @ [currentBoard]) Map.empty 0 (boardNumber + 1) xs
                else
                    let currentBoard =
                        Regex.Matches(row, "\\d+")
                        |> Seq.mapi (fun x v -> (x, y), (int v.Value, false))
                        |> Seq.fold (fun cb (key, value) -> Map.add key value cb) currentBoard
                    getBoards boards currentBoard (y + 1) boardNumber xs
        getBoards [] Map.empty 0 0 (List.skip 1 lines) 
    
    let boards, numbers = getSingle 2021 4 (string >> parse)

    let isBingo board =
        List.exists id [
            [ 0..4 ] |> List.exists (fun i -> [0..4] |> List.forall (fun j -> Map.find (i, j) board |> snd))
            [ 0..4 ] |> List.exists (fun i -> [0..4] |> List.forall (fun j -> Map.find (j, i) board |> snd))
        ]

    let updateBoards boards exclNo =
        List.map (fun board -> 
            match Map.tryFindKey (fun _ (v, _) -> v = exclNo) board with
            | Some key -> Map.add key (exclNo, true) board
            | None -> board) boards

    let findFirstWinner boards numbers =
        let rec turn boards = function
            | [] -> failwithf "went through all numbers, no bingo"
            | exclNo :: xs ->
                let boards = updateBoards boards exclNo
                List.tryPick (fun board -> if isBingo board then Some board else None) boards
                |> function
                    | None -> turn boards xs
                    | Some board -> board, exclNo
        turn boards numbers

    let findLastWinner boards numbers =
        let rec turn boards winners lastWinExclNo = function
            | [] -> List.last winners, lastWinExclNo
            | exclNo :: xs ->
                let boards = updateBoards boards exclNo
                List.choose (fun board -> if isBingo board then Some board else None) boards
                |> function
                    | [] -> turn boards winners lastWinExclNo xs
                    | newWinners ->
                        let boards = List.filter (fun b -> not (List.contains b newWinners)) boards
                        turn boards (winners @ newWinners) exclNo xs
        turn boards [] 0 numbers

    let calcWinner (board: Board, exclNo: int) =
        Map.toList board
        |> List.filter (fun (_, (_, marked)) -> not marked)
        |> List.sumBy (fun (_, (v, _)) -> v)
        |> fun sum -> sum * exclNo

    let part1() = findFirstWinner boards numbers |> calcWinner
    let part2() = findLastWinner boards numbers |> calcWinner

    let solve () = printDay 2021 3 part1 part2