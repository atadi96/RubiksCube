namespace RubiksCube.Core

module Cube =
    open System.Drawing

    //[<StructuralEquality; StructuralComparison>]
    [<CustomEquality; NoComparison>]
    type CubeColor =
        | CWhite
        | CRed
        | CGreen
        | COrange
        | CBlue
        | CYellow with
        member this.Opposite =
            match this with
            | CWhite -> CYellow
            | CYellow -> CWhite
            | CRed -> COrange
            | COrange -> CRed
            | CBlue -> CGreen
            | CGreen -> CBlue
        override this.Equals (other: obj) =
            match other with
            | :? CubeColor as other ->
                match this, other with
                | CWhite, CWhite
                | CRed, CRed
                | CGreen, CGreen
                | COrange, COrange
                | CBlue, CBlue
                | CYellow, CYellow ->
                    true
                | _ -> false
            | _ -> false
        override this.GetHashCode() =
            match this with
            | CWhite -> 2
            | CRed -> 3
            | CGreen -> 5
            | COrange -> 7
            | CBlue -> 11
            | CYellow -> 13


    module CubeColor =
        let opposite (color: CubeColor) = color.Opposite

    type Corner =
        | WhiteRedGreen
        | WhiteBlueRed
        | WhiteOrangeBlue
        | WhiteGreenOrange
        | YellowGreenRed
        | YellowOrangeGreen
        | YellowBlueOrange
        | YellowRedBlue

    type Edge =
        | WhiteRed
        | WhiteBlue
        | WhiteOrange
        | WhiteGreen
        | YellowRed
        | YellowBlue
        | YellowOrange
        | YellowGreen
        | OrangeBlue
        | BlueRed
        | RedGreen
        | GreenOrange

    [<CustomEquality; NoComparison>]
    type Face = Face of CubeColor * (CubeColor * CubeColor * CubeColor * CubeColor * CubeColor * CubeColor * CubeColor * CubeColor) with
        member this.Fields =
            match this with
            | Face (_,(f0,f1,f2,f3,f4,f5,f6,f7)) ->
                seq {
                    yield f0; yield f1; yield f2; yield f3
                    yield f4; yield f5; yield f6; yield f7
                }

        override this.GetHashCode() =
            let (Face (middle,_)) = this
            this.Fields
            |> Seq.fold (fun prev field ->
                let fieldHash = field.GetHashCode()
                prev * fieldHash + fieldHash
            ) 1
            |> fun hash -> hash <<< (middle.GetHashCode() - 2)

        override this.Equals (other: obj) =
            match other with
            | :? Face as other ->
                Seq.forall2 (=) this.Fields other.Fields
            | _ -> false

    type Line = Line of CubeColor * CubeColor * CubeColor

    type Patch = Patch of (Line * Line * Line * Line)

    module Patch =
        let turn (direction: TurnDirection) (Patch (a,b,c,d)) =
            match direction with
            | Clockwise -> Patch (d,a,b,c)
            | CounterClockwise -> Patch (b,c,d,a)

    type LinePos =
        | Top
        | Right
        | Bottom
        | Left

    module LinePos =
        let opposite = function
            | Top -> Bottom
            | Bottom -> Top
            | Left -> Right
            | Right -> Left

    type Side = Side of CubeColor * Face * Patch 

    module Face =
        let createColor (c: CubeColor) =
            Face (c,(c,c,c,c,c,c,c,c))

        let fields (face: Face) = face.Fields

        let flipVert (Face (m,(f00,f01,f02,f12,f22,f21,f20,f10)) as face) =
            Face (m,(f20,f21,f22,f12,f02,f01,f00,f10))

        let private posDrop (pos: LinePos) =
            match pos with
            | Top -> 0
            | Right -> 1
            | Bottom -> 2
            | Left -> 3
            * 2

        let getLine (pos: LinePos) (face: Face): Line =
            let (Face (middle,_)) = face
            let drop =
                pos
                |> posDrop
            face
            |> fields
            |> Seq.replicate 2
            |> Seq.concat
            |> Seq.skip drop
            |> Seq.take 3
            |> Seq.toList
            |> fun [a; b; c] ->
                let tuple =
                    if middle = CYellow then
                        (c,b,a)
                    else
                        (a,b,c)
                //let tuple = (a,b,c)
                tuple
            |> Line

        let setLine (pos: LinePos) (Line (a,b,c): Line) (face: Face): Face =
            let (Face (color,_)) = face
            let (a,b,c) =
                match color with
                | CYellow -> (c,b,a)
                |_ -> (a,b,c)
            let fields =
                face
                |> fields
                |> Seq.replicate 2
                |> Seq.concat
                |> Seq.toList
            let drop = posDrop pos
            let [f0;f1;f2;f3;f4;f5;f6;f7] =
                seq {
                    yield! fields |> List.take drop
                    yield a; yield b; yield c
                    yield! fields |> List.skip (drop+3)
                }
                |> Seq.take 8
                |> Seq.toList
            let f0 =
                if drop = 6 then
                    c
                else
                    f0
            let face = Face (color,(f0,f1,f2,f3,f4,f5,f6,f7))
            face

        let turnFace (direction: TurnDirection) (face: Face) =
            let (Face (color,(a,b,c,d,e,f,g,h))) = face
            match direction with
            | Clockwise -> Face (color,(g,h,a,b,c,d,e,f))
            | CounterClockwise -> Face (color,(c,d,e,f,g,h,a,b))

        let wrongBlackEdges (Face (f11,(f00,f01,f02,f12,f22,f21,f20,f10))) =
            [|
                f00 = f01; f01 = f02
                f00 = f10; f01 = f11; f02 = f12
                f10 = f11; f11 = f12;
                f10 = f20; f11 = f22; f12 = f22
                f20 = f21; f21 = f22
            |]
            |> Seq.filter not
            |> Seq.length

    type Tuple4< ^T> = ^T * ^T * ^T * ^T

    module Tuple4 =
        let toList (a,b,c,d) = [a;b;c;d]
        let toArray (a,b,c,d) = [|a;b;c;d|]
        let sortBy f tup =
            let ar = tup |> toArray
            ar |> Array.sortInPlaceBy f
            let [|a;b;c;d|] = ar
            (a,b,c,d)
        let reverse (a,b,c,d) = (d,c,b,a)
        let zip (a0,b0,c0,d0) (a1,b1,c1,d1) = (a0,a1),(b0,b1),(c0,c1),(d0,d1)
        let toSeq (a,b,c,d) = seq { yield a; yield b; yield c; yield d }
        let map f (a,b,c,d) = (f a, f b, f c, f d)
        let fold f s (a, b, c, d) =
            [|a; b; c; d|]
            |> Array.fold f s

    module Side =
        let turn (direction: TurnDirection) (Side (color,face,lines): Side): Side =
            Side ( color
                 , Face.turnFace direction face
                 , Patch.turn direction lines )

    [<CustomEquality; NoComparison>]
    type Cube =
        {
            white : Face
            red : Face
            green : Face
            yellow : Face
            blue : Face
            orange : Face
        }
        member this.Faces =
            seq {
                yield this.white; yield this.red; yield this.green
                yield this.yellow; yield this.blue; yield this.orange
            }
        override this.GetHashCode() =
            this.Faces
            |> Seq.map (fun x -> x.GetHashCode())
            |> Seq.fold (fun a b -> a + b) 0
        override this.Equals (other: obj) =
            match other with
            | :? Cube as other ->
                Seq.forall2 (=) this.Faces other.Faces
            | _ -> false

    module Cube =
        open Face

        let finished =
            {
                white = createColor CWhite
                red = createColor CRed
                green = createColor CGreen
                yellow = createColor CYellow
                blue = createColor CBlue
                orange = createColor COrange
            }

        let face (color: CubeColor) (cube: Cube) =
            match color with
            | CWhite -> cube.white
            | CGreen -> cube.green
            | CRed -> cube.red
            | CBlue -> cube.blue
            | COrange -> cube.orange
            | CYellow -> Face.flipVert cube.yellow

        let updateFace (update: Face -> Face) (color: CubeColor) (cube: Cube) =
            match color with
            | CWhite -> { cube with white = update cube.white }
            | CGreen -> { cube with green = update cube.green }
            | CRed -> { cube with red = update cube.red }
            | CBlue -> { cube with blue = update cube.blue }
            | COrange -> { cube with orange = update cube.orange }
            | CYellow -> { cube with yellow = (Face.flipVert << update << Face.flipVert) cube.yellow }

        let setFace (face: Face) (cube: Cube) =
            let (Face (color,_)) = face
            match color with
            | CWhite -> { cube with white = face }
            | CGreen -> { cube with green = face }
            | CRed -> { cube with red = face }
            | CBlue -> { cube with blue = face }
            | COrange -> { cube with orange = face }
            | CYellow -> { cube with yellow = Face.flipVert face }

        let neighbours (color: CubeColor): Tuple4<CubeColor * LinePos> =
            let horizontal x = (CBlue,x),(CRed,x),(CGreen,x),(COrange,x)
            let vertical where left right = (CWhite,where),(right,Left),(CYellow,LinePos.opposite where),(left,Right)
            match color with
            | CWhite -> horizontal Top
            | CGreen -> vertical Bottom COrange CRed
            | CRed -> vertical Right CGreen CBlue
            | CBlue -> vertical Top CRed COrange
            | COrange -> vertical Left CBlue CGreen
            | CYellow -> horizontal Bottom //|> Tuple4.reverse

        let getSide (cube: Cube) (color: CubeColor): Side =
            let patch =
                color
                |> neighbours
                |> Tuple4.map (fun (color,pos) ->
                    let face =
                        cube
                        |> face color
                    getLine pos face
                )
                |> Patch
            Side (color, cube |> face color, patch)

        let setSide (cube: Cube) (Side (color,face,Patch lines): Side) =
            let cube =
                cube
                |> setFace face
            neighbours color
            |> Tuple4.zip lines
            |> Tuple4.fold (fun cube (line,(color,pos)) ->
                updateFace (Face.setLine pos line) color cube
            ) cube

        let private turnData (turn:Turn): CubeColor * TurnDirection =
            let cw color = color,Clockwise
            let cc color = color,CounterClockwise
            match turn with
            | Up -> cw CWhite
            | Down -> cc CYellow
            | Turn.Left -> cw COrange
            | Turn.Right -> cw CRed
            | Front -> cw CGreen
            | Back -> cw CBlue
            | Up' -> cc CWhite
            | Down' -> cw CYellow
            | Left' -> cc COrange
            | Right' -> cc CRed
            | Front' -> cc CGreen
            | Back'-> cc CBlue

        let turn (turn:Turn) (cube: Cube) =
            let (color,direction) =
                turn
                |> turnData
            color
            |> getSide cube
            |> Side.turn direction
            |> setSide cube

        let wrongBlackEdges (cube:Cube) =
            cube.Faces
            |> Seq.map Face.wrongBlackEdges
            |> Seq.sum
