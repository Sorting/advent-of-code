module Day90

open Utilities

type Lottery = { people: float
                 winners: float 
                 tickets: float
                 peopleInGroup: float }

let lottery =  
    getSingle 90 (fun x -> x.Split(' ')) 
    |> function 
    | [| people; winners; tickets; peopleInGroup |] -> 
        { people = float people 
          winners = float winners
          tickets = float tickets
          peopleInGroup = float peopleInGroup }
    | _ ->
        { people = 0.
          winners = 0.
          tickets = 0.
          peopleInGroup = 0. }

let getProbability =
    lottery.winners / ((lottery.people+0.7) / lottery.peopleInGroup)

let solve() =
    printfn "%f" getProbability