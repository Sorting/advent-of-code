module Day04

// open System
// open Utilities

// type AwakeState = Awake | Asleep
// type Guard = { state: AwakeState; date: DateTime; guardId: int}

// let parser = function
// | Regex @"^\[(.*?)\] Guard #(\d+) begins shift$" [ date; id ] -> 
//     let date = Convert.ToDateTime date
//     let beginDate = 
//         if date.Hour = 23 
//         then DateTime(date.AddDays(1).Year, date.AddDays(1).Month, date.AddDays(1).Day, 0, 0, 0)
//          else date

//     { state = Awake; guardId = int id; date = beginDate }
// | Regex @"^\[(.*?)\] Guard #(\d+) begins shift$" [ date; id ] -> 
//     { state = Awake; guardId = int id; date = Convert.ToDateTime date }
// | _ -> 
//     { state = Awake; guardId = 0; date = DateTime() }

// let part1 = 
//     getMany 4 parser
//     |> Seq.sortBy (fun x -> x.date)
//     |> Seq.groupBy (fun x -> x.date)
//     |> Seq.map (fun (key, values) ->
//         Seq.fold (fun (map, startMinute) x -> ) (Map.empty<int * int, AwakeState>, 0) values)