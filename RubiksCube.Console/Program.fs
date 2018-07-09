namespace RubiksCube.Console

open System
open RubiksCube.Core.Cube

module Main =

    [<EntryPoint>]
    let main _argv =
        //CubeProgram.test ()
        CubeProgram.run ()
        0
