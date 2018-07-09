namespace RubiksCube.Console

open System

type Shader = private | Shader of int * int * seq<seq<ConsoleColor>>

module Shader =
    let ofBitmap' (bitMap: ConsoleColor[,]) =
        let height = bitMap.GetLength 0
        let width = bitMap.GetLength 1
        let bmEnum =
            seq {
                let mutable y = 0
                while y < height do
                    yield seq {
                        let mutable x = 0
                        while x < width do
                            yield bitMap.[x,y]
                            x <- x + 1
                        }
                    y <- y + 1
            }
        Shader (width, height, bmEnum)

    let ofBitmap (bitMap: ConsoleColor[][]) =
        let height = bitMap.Length
        let width = bitMap.[0].Length
        let bmEnum =
            seq {
                let mutable y = 0
                while y < height do
                    yield seq {
                        let mutable x = 0
                        while x < width do
                            yield bitMap.[y].[x]
                            x <- x + 1
                        }
                    y <- y + 1
            }
        Shader (width, height, bmEnum)

    let private crop x y (pixels: seq<seq<ConsoleColor>>) =
        pixels
        |> Seq.take y
        |> Seq.map (Seq.take x)

    let private verticalInfinite (shader: seq<seq<ConsoleColor>>) =
        Seq.initInfinite (fun _ -> Seq.initInfinite (fun _ -> ConsoleColor.Black))
        |> Seq.append shader

    let private horizontalInfinite (shader: seq<seq<ConsoleColor>>) =
        shader
        |> Seq.map (fun en -> 
            Seq.initInfinite (fun _ -> ConsoleColor.Black)
            |> Seq.append en
        )

    let (<=>) (Shader (w1,h1,en1)) (Shader (w2,h2,en2)) =
        let sideBySide =
            let en1 = en1 |> verticalInfinite
            let en2 = en2 |> verticalInfinite
            Seq.zip en1 en2
            |> Seq.map (fun (a,b) -> Seq.append a b)
        Shader (w1+w2, max h1 h2, sideBySide)

    let (<||>) (Shader (w1,h1,en1)) (Shader (w2,h2,en2)) =
        let width = max w1 w2
        let height = h1 + h2
        let under =
            Seq.append
                (horizontalInfinite en1)
                (horizontalInfinite en2)
            |> Seq.map (Seq.take width)
        Shader (width, height, under)

    let transform x y (Shader (w,h,pixels)) =
        let width = w+x
        let height = h+y
        let newPixels: seq<seq<ConsoleColor>> = 
            Seq.append
                (Seq.init y (fun _ -> Seq.initInfinite (fun _ -> ConsoleColor.Black)))
                (
                    pixels
                    |> Seq.map (
                        Seq.append (
                            Seq.init x (fun _ -> ConsoleColor.Black)
                        )
                    )
                )
            |> crop width height
        Shader (width, height, newPixels)

    let (<^>) (Shader (w1,h1,en1)) (Shader (w2,h2,en2)) =
        let lower =
            en1
            |> horizontalInfinite
            |> verticalInfinite
        let higher =
            en2
            |> horizontalInfinite
            |> verticalInfinite
        let width = max w1 w2
        let height = max h1 h2
        let pixels' =
            Seq.zip lower higher
            |> Seq.map (fun (lowRow,highRow) ->
                Seq.zip lowRow highRow
                |> Seq.map (fun (lowPixel,highPixel) ->
                    if highPixel = ConsoleColor.Black then
                        lowPixel
                    else
                        highPixel
                )
            )
            |> crop width height
        let pixels =
            let lower = lower.GetEnumerator()
            let higher = higher.GetEnumerator()
            seq {
                let mutable y = 0
                while y < height do
                    yield seq {
                        let mutable x = 0
                        let higherRow = higher.Current.GetEnumerator()
                        let lowerRow = lower.Current.GetEnumerator()
                        while x < width do
                            let higherPixel = higherRow.Current
                            yield
                                if higherPixel = ConsoleColor.Black then
                                    lowerRow.Current
                                else
                                    higherPixel
                            higherRow.MoveNext() |> ignore
                            lowerRow.MoveNext() |> ignore
                            x <- x + 1
                        }
                    higher.MoveNext() |> ignore
                    lower.MoveNext() |> ignore
                    y <- y + 1
            }
        Shader (width,height,pixels')

    let stretchHorizontal (Shader (x,y,pixels)): Shader =
        let width = 2*x
        let height = y
        let newPixels =
            pixels
            |> Seq.map (fun row ->
                let enum = row.GetEnumerator()
                seq {
                    while enum.MoveNext() do
                        yield enum.Current
                        yield enum.Current
                }
            )
        Shader (width, height, newPixels)

    let draw (Shader (x,y,pixels)): unit =
        pixels
        |> Seq.take y
        |> Seq.iter (fun row ->
            row
            |> Seq.take x
            |> Seq.iter (fun color ->
                Console.BackgroundColor <- color
                Console.Write ' '
            )
            Console.WriteLine()
        )
        Console.BackgroundColor <- ConsoleColor.Black


