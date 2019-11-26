namespace Year2015

module Day02 =
    open Utilities

    let parsePresent = function
    | Regex @"^(\d+)x(\d+)x(\d+)$" [l; w; h] -> int l, int w, int h
    | s -> failwithf "%s is not a correct definition of a present" s

    let presents = getMany 2015 2 parsePresent

    let part1() = Seq.sumBy (fun (l, w, h) -> 2 * (l*w) + 2 * (w*h) + 2 * (l*h) + (min (min (l*w) (w*h)) (l*h))) presents
    let part2() = Seq.sumBy (fun (l, w, h) -> (min (min (l+l+w+w) (w+w+h+h)) (l+l+h+h)) + (l*w*h)) presents
    
    let solve() = printDay 2015 2 part1 part2