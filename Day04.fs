module Day04

open System
open Utilities

type Record = { asleep: int; date: DateTime; guardId: int}

let parser = function
| Regex @"^\[(.*?)\] Guard #(\d+) begins shift$" [ date; id ] -> 
    let date = Convert.ToDateTime date
    let date = if date.Hour = 23 then date.AddDays(1.).Date else date
    { asleep = 0
      guardId = int id
      date = date }
| Regex @"^\[(.*?)\] falls asleep$" [ date ] -> 
    { asleep = 1
      guardId = 0
      date = Convert.ToDateTime date }
| Regex @"^\[(.*?)\] wakes up$" [ date ] -> 
    { asleep = 0
      guardId = 0
      date = Convert.ToDateTime date }
| _ -> { asleep = 0
         guardId = 0
         date = DateTime() }

let records = 
    getMany 4 parser
    |> Seq.sortBy (fun x -> x.date)
    |> Seq.groupBy (fun x -> x.date.Date)
    |> Seq.map (fun (_, records) -> 
        let id = Seq.map (fun x -> x.guardId) records |> Seq.max
        id, 
        Seq.map (fun x -> { x with guardId = id }) records
        |> Seq.pairwise
        |> Seq.collect (fun (first, second) -> 
            [ for i in first.date.Minute..second.date.Minute-1 -> 
                (i, first.asleep) ]
        ))
    |> Seq.groupBy (fun (id, _) -> id)

let part1() =
    records
    |> Seq.maxBy (fun (_, records) ->
        Seq.sumBy (fun (_, minutes) -> minutes |> Seq.sumBy snd) records)
    |> fun (id, records) ->
        let minute = 
            Seq.collect (fun (_, minutes) -> minutes) records
            |> Seq.groupBy (fun (minute, _) -> minute)
            |> Seq.maxBy (fun (_, minutes) -> 
                Seq.filter (fun (_, asleep) -> asleep = 1) minutes 
                |> Seq.length)
            |> fun (minute, _) -> minute
        id * minute

let part2() =
    records
    |> Seq.map (fun (id, records) ->
        let groupedMinutes = 
            Seq.collect (fun (_, minutes) -> minutes) records
            |> Seq.groupBy (fun (minute, _) -> minute)
        if Seq.isEmpty groupedMinutes 
        then (id, 0, 0)
        else
            let minute, count = 
                Seq.maxBy (fun (_, minutes) -> 
                    Seq.filter (fun (_, asleep) -> asleep = 1) minutes
                    |> Seq.length) groupedMinutes
                |> fun (minute, minutes) -> 
                    minute, 
                    minutes 
                    |> Seq.filter (fun (_, asleep) -> asleep = 1) 
                    |> Seq.length
            id, minute, count)
    |> Seq.maxBy (fun (_, _, count) -> count)
    |> fun (id, minute, _) -> id * minute

let solve() = printDay 4 part1 part2