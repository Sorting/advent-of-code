namespace Year2020

module Day16 =
    open System
    open Utilities

    type Rule = RangeRule of int * int

    type TicketTranslator =
        { Rules: Rule list 
          NearTickets: int list }

    let rec parse ticketTranslator = function
        | [] -> ticketTranslator
        | x :: xs ->
            match x with
            | Regex "([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" [firstMin; firstMax; secondMin; secondMax] ->
                parse { ticketTranslator with Rules = RangeRule(int firstMin, int firstMax) :: RangeRule(int secondMin, int secondMax) :: ticketTranslator.Rules } xs
            | Regex @"^(\d+,)+(\d+)$" _ ->
                let values = x.Split(',') |> List.ofArray |> List.map int
                parse { ticketTranslator with NearTickets = ticketTranslator.NearTickets @ values } xs
            | _ -> parse ticketTranslator xs
    
    let ticketTranslator = getMany 2020 16 string |> Seq.toList |> parse { Rules = []; NearTickets = [] }

    let rec tryInvalidValue value = function
        | [] -> Some value
        | RangeRule(min, max) :: xs ->
            if value >= min && value <= max then None
            else tryInvalidValue value xs

    let part1() = 
        ticketTranslator.NearTickets
        |> List.choose (fun value -> tryInvalidValue value ticketTranslator.Rules)
        |> List.sum

    let part2() = 0

    let solve () = printDay 2020 16 part1 part2
