namespace Year2019

module Day01 =
    open Utilities

    let masses = getMany 2019 1 (double)
    let fuelFormula mass = int (floor (mass / 3.) - 2.)

    let rec calcFuel fuel mass =
        match fuelFormula mass with
        | currentFuel when currentFuel > 0 -> calcFuel (fuel + currentFuel) (double currentFuel)
        | _ -> fuel

    let part1 () = Seq.sumBy fuelFormula masses
    let part2 () = Seq.sumBy (calcFuel 0) masses

    let solve () = printDay 2019 1 part1 part2
