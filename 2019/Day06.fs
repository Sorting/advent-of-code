namespace Year2019

module Day06 =    
    open Utilities

    type PlanetPair = PlanetPair of string * string

    let parser (s: string) =
        let [| a; b |] = s.Split(")")
        PlanetPair (a, b)

    let addOrUpdate (map: Map<string, string list>) = function
        | PlanetPair(a, b) -> map
        | _ -> map

    
    let rec buildMap (map: Map<string, string>) = function
        | PlanetPair(a, b) ->
            map


    let map = 
        getMany 2019 6 parser



    let part1() = 0
    let part2() = 0

    let solve() = printDay 2019 6 part1 part2