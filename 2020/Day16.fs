namespace Year2020

module Day16 =
    open Utilities

    type Rule = RangeRule of string * int * int

    type TicketTranslator =
        { Rules: Rule list 
          Tickets: int list list }

    let rec parse ticketTranslator = function
        | [] -> ticketTranslator
        | x :: xs ->
            match x with
            | Regex @"^(.*?)\: ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$" [field; firstMin; firstMax; secondMin; secondMax] ->
                parse { ticketTranslator with Rules = RangeRule(field, int firstMin, int firstMax) :: RangeRule(field, int secondMin, int secondMax) :: ticketTranslator.Rules } xs
            | Regex @"^(\d+,)+(\d+)$" _ ->
                let values = x.Split(',') |> List.ofArray |> List.map int
                parse { ticketTranslator with Tickets = ticketTranslator.Tickets @ [ values ] } xs
            | _ -> parse ticketTranslator xs
    
    let ticketTranslator = getMany 2020 16 string |> Seq.toList |> parse { Rules = []; Tickets = [] }

    let rec findInvalidValue value reverse = function
        | [] -> if reverse then None else Some value
        | RangeRule(_, min, max) :: xs ->
            if value >= min && value <= max then if reverse then Some value else None 
            else findInvalidValue value reverse xs

    let rec findCandidates value candidateFields = function
        | [] -> candidateFields 
        | RangeRule(field, min, max) :: xs ->
            if value >= min && value <= max then findCandidates value (field :: candidateFields) xs
            else findCandidates value candidateFields xs

    let part1() = 
        ticketTranslator.Tickets
        |> List.collect (fun values -> values |> List.choose (fun value -> findInvalidValue value false ticketTranslator.Rules))
        |> List.sum

    let part2() = 
        let myTicket = List.head ticketTranslator.Tickets
        let validNearbyTickets =
            ticketTranslator.Tickets
            |> List.skip 1
            |> List.filter (fun values -> 
                values
                |> List.choose (fun value -> findInvalidValue value true ticketTranslator.Rules)
                |> List.length = List.length values)

        validNearbyTickets
        |> List.collect (fun values -> 
            values 
            |> List.mapi (fun i value -> 
                findCandidates value [] ticketTranslator.Rules 
                |> List.map (fun field -> i, field))
            |> List.collect id)
        |> List.groupBy snd
        |> List.map (fun (field, list) -> 
            field, 
            list
            |> List.countBy fst
            |> List.filter (snd >> (=) (List.length validNearbyTickets))
            |> List.map fst)
        |> List.sortBy (snd >> List.length) 
        |> List.fold (fun map (field, candidates) ->
            candidates
            |> List.filter (fun idx -> not (Map.containsKey idx map))
            |> List.head
            |> fun idx -> Map.add idx field map) Map.empty
        |> Map.toList
        |> List.filter (fun (_, field) -> field.Contains("departure"))
        |> List.map (fun (idx, _) -> int64 (List.item idx myTicket))
        |> List.reduce (*) 

    let solve () = printDay 2020 16 part1 part2
