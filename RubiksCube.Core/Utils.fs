namespace RubiksCube.Core

module Utils =
    module List =
        let rec intersparse elem = function
            | [] -> []
            | x :: xs -> x :: elem :: intersparse elem xs

    let inline flip< ^T, ^U, ^V> (f : ^T -> ^U -> ^V) (a: ^U) (b: ^T): ^V = f b a

