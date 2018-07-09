namespace RubiksCube.Core

    type Turn =
        | Up
        | Down
        | Left
        | Right
        | Front
        | Back
        | Up'
        | Down'
        | Left'
        | Right'
        | Front'
        | Back'
        override this.ToString() =
            match this with
            | Up -> "U"
            | Down -> "D"
            | Left -> "L"
            | Right -> "R"
            | Front -> "F"
            | Back -> "B"
            | Up' -> "U'"
            | Down' -> "D'"
            | Left' -> "L'"
            | Right' -> "R'"
            | Front' -> "F'"
            | Back' -> "B'"

    module Turn =
        let all =
            [
                Up
                Down
                Left
                Right
                Front
                Back
                Up'
                Down'
                Left'
                Right'
                Front'
                Back'
            ]

        let opposite = function
            | Up -> Up'
            | Down -> Down'
            | Left -> Left'
            | Right -> Right'
            | Front -> Front'
            | Back -> Back
            | Up' -> Up
            | Down' -> Down
            | Left' -> Left
            | Right' -> Right
            | Front' -> Front
            | Back' -> Back

        let randomSequence length =
            let random = System.Random()
            let randomElem (elems: 'a[]) =
                let index = random.Next(elems.Length)
                elems.[index]
            let randomExcept (except: 'a) (elems: 'a[]) =
                let index = random.Next(elems.Length - 1)
                let firstTry = elems.[index]
                if firstTry = except then
                    elems.[index+1]
                else
                    firstTry
            let turns = all |> Array.ofList
            Error length
            |> List.unfold (function
                | Error length ->
                    if length > 0 then
                        let turn = turns |> randomElem
                        Some (turn, Ok (turn,length - 1))
                    else
                        None
                | Ok (_,0) -> None
                | Ok (last,length) ->
                    let turn =
                        turns
                        |> randomExcept (opposite last)
                    Some (turn, Ok (turn,length-1))
            )

    type TurnDirection =
        | Clockwise
        | CounterClockwise

