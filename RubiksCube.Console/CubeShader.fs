namespace RubiksCube.Console

module CubeShader =
    open System
    open RubiksCube.Core
    open RubiksCube.Core.Cube

    let private cubeToConsoleColor (color:CubeColor) =
        match color with
        | CWhite -> ConsoleColor.White
        | CBlue -> ConsoleColor.Blue
        | CGreen -> ConsoleColor.Green
        | COrange -> ConsoleColor.DarkRed
        | CRed -> ConsoleColor.Red
        | CYellow -> ConsoleColor.Yellow

    let private transformColors (pixels:CubeColor option [] []) =
        pixels
        |> Array.map (Array.map (fun field ->
            match field with
            | Some c -> cubeToConsoleColor c
            | None -> ConsoleColor.Black
        ))

    let private diamondFace (Face (f11,(f00,f01,f02,f12,f22,f21,f20,f10)): Face) =
        let o = None
        let f00 = Some f00
        let f01 = Some f01
        let f02 = Some f02
        let f10 = Some f10
        let f11 = Some f11
        let f12 = Some f12
        let f20 = Some f20
        let f21 = Some f21
        let f22 = Some f22
        let bitmap: ConsoleColor[][] =
            [|
                [|  o;   o; f00; o;     o|]
                [|  o; f10;   o; f01;   o|]
                [|f20;   o; f11;   o; f02|]
                [|  o; f21;   o; f12;   o|]
                [|  o;   o; f22;   o;   o|]
            |]
            |> transformColors
        bitmap |> Shader.ofBitmap

    let private leftFace (Face (f11,(f00,f01,f02,f12,f22,f21,f20,f10)): Face) =
        let o = None
        let f00 = Some f00
        let f01 = Some f01
        let f02 = Some f02
        let f10 = Some f10
        let f11 = Some f11
        let f12 = Some f12
        let f20 = Some f20
        let f21 = Some f21
        let f22 = Some f22
        let bitmap: ConsoleColor[][] =
            [|
                [|f00;   o;   o|]
                [|f10; f01;   o|]
                [|f20; f11; f02|]
                [|  o; f21; f12|]
                [|  o;   o; f22|]
            |]
            |> transformColors
        bitmap
        |> Shader.ofBitmap

    let private rightFace (Face (f11,(f00,f01,f02,f12,f22,f21,f20,f10)): Face) =
        let o = None
        let f00 = Some f00
        let f01 = Some f01
        let f02 = Some f02
        let f10 = Some f10
        let f11 = Some f11
        let f12 = Some f12
        let f20 = Some f20
        let f21 = Some f21
        let f22 = Some f22
        let bitmap: ConsoleColor[][] =
            [|
                [|  o;   o; f02|]
                [|  o; f01; f12|]
                [|f00; f11; f22|]
                [|f10; f21;   o|]
                [|f20;   o;   o|]
            |]
            |> transformColors
        bitmap
        |> Shader.ofBitmap

    let private fixYellox (Face (m,(a,b,c,d,e,f,g,h)): Face) =
        (*
        00 01 02
        10 11 12
        20 21 22
            v
        22 12 02
        21 11 01
        20 10 00
        *)
        Face (m,(a,h,g,f,e,d,c,b))

    open Shader

    let cubeShader (cube: Cube): Shader =
        let green =
            cube.green
            |> leftFace
            |> transform 0 3
        let white =
            cube.white
            |> diamondFace
            |> transform 1 0
        let red =
            cube.red
            |> rightFace
            |> transform 4 3
        let blue =
            cube.blue
            |> rightFace
        let yellow =
            cube.yellow
            |> Face.turnFace CounterClockwise
            //|> fixYellox
            |> diamondFace
            |> transform 1 3
        let orange =
            cube.orange
            |> leftFace
            |> transform 4 0
        (green <^> white <^> red) <=> transform 2 0 (orange <^> yellow <^> blue)
