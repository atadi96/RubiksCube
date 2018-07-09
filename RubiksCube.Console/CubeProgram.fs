namespace RubiksCube.Console

open RubiksCube.Core.Cube
open RubiksCube.Core
open RubiksCube.Core.Utils
open System

module CubeProgram =

    type private Command =
        | Turns of Turn list
        | Solve
        | Exit
        | Reset
        | Help
        | Start

    type private State = Cube * Result<Command,string>

    open Parser

    let private parseTurn =
        (token "u'" *> constant Up') <|>
        (token "l'" *> constant Left') <|>
        (token "r'" *> constant Right') <|>
        (token "d'" *> constant Down') <|>
        (token "f'" *> constant Front') <|>
        (token "b'" *> constant Back') <|>
        (token "u" *> constant Up) <|>
        (token "l" *> constant Left) <|>
        (token "r" *> constant Right) <|>
        (token "d" *> constant Down) <|>
        (token "f" *> constant Front) <|>
        (token "b" *> constant Back)
    in
    let private exit = (token "exit" <|> token "quit" <|> token "q") *> constant Exit
    in
    let private reset = token "reset" *> constant Reset
    in
    let private turns = Turns <%> (whitespace *> any (parseTurn <* whitespace))
    in
    let private solve = constant Solve <* token "solve"
    in
    let private help = token "help" *> constant Help
    in
    let private commands = reset <|> exit <|> solve <|> turns <|> help

    let solveCubeBreadth (cube: Cube): (Cube * Turn list) option =
        BreadthSolver.search
            40
            cube
            (fun cube ->
                Turn.all
                |> List.map (fun turn -> cube |> Cube.turn turn, turn)
            )
            (fun cube -> cube = Cube.finished)
            -1
            (fun currentLevel node ->
                let depth = node.Depth
                if depth > currentLevel then
                    printfn "Current depth: %i" depth
                    depth
                else
                    currentLevel
            )
        |> Option.map (fun (_start,x,finish) -> finish, (x |> List.map (fun (x,y) -> x)))

    let aStarProblem (start: Cube) (finished: Cube) : AStar.Problem<Cube,Turn> =
        {
            startNode = start
            edges =  fun cube ->
                Turn.all
                |> Seq.map (fun turn -> cube |> Cube.turn turn, (turn, 1.0))
            heuristics = fun cube ->
                cube
                |> Cube.wrongBlackEdges
                |> float
                |> fun edges -> edges / 60.0
            endCondition = fun cube -> cube = finished
        }

    let solveAStar (cube: Cube): (Cube * Turn list) option =
        aStarProblem cube Cube.finished
        |> AStar.solve
        |> Option.map (fun (_start,steps,finish) ->
            finish, (steps |> List.map (fun (turn,_cube) -> turn))
        )

    let rec private program ((cube,read):State) =
        Console.Clear()

        let newState: (Cube * string) option =
            match read with
            | Ok (Turns turns) ->
                (
                    turns |> List.fold (flip Cube.turn) cube
                    ,
                    turns
                    |> List.map (fun x -> x.ToString())
                    |> List.intersparse " "
                    |> List.fold (+) ""
                    |> sprintf "Ok. Last: %s"
                )
                |> Some
            | Ok Start -> Some (Cube.finished, "Interactive loaded. Type 'help' for available commands.")
            | Ok Help ->
                let text =
                    "Commands:\n"
                    + "\thelp - display this message\n"
                    + "\tq | quit | exit - exit the application\n"
                    + "\treset - reset the cube\n"
                    + "\tsolve - solve the cube from the current state\n\n"
                    + "Turning the cube: upper- or lowercase first letter of the sides for clockwise\n"
                    + "Add ' character for counterclocwise\n"
                    + "Sides: Front, Back, Up, Down, Left, Right\n"
                    + "Example: f d' r'Lu B"
                Some (cube, text)
            | Ok Reset -> Some (Cube.finished, "Ok. Cube reset.")
            | Ok Exit -> None
            | Ok Solve ->
                cube
                |> solveAStar
                |> function
                    | None -> Some (cube, "No solution :(")
                    | Some (finish,turns) ->
                        (
                            finish
                            ,
                            turns
                            |> List.map (fun x -> x.ToString())
                            |> List.intersparse " "
                            |> List.fold (+) ""
                            |> sprintf "Ok. Solution: %s"
                        )
                        |> Some
            | Error text -> Some (cube, sprintf "Error parsing \"%s\"" text)
        match newState with
        | Some (cube,message) ->
            //let cube = { cube with yellow = Face (CYellow,(CWhite,CGreen,CRed,CBlue,CYellow,COrange,CBlue,CRed))}
            cube
            |> CubeShader.cubeShader
            |> Shader.draw

            printfn "%s" message

            let input =
                Console.ReadLine()

            let turns =
                input.ToLower()
                |> runParser commands
                |> function
                    | Some turns -> Ok turns
                    | None -> Error input

            program (cube,turns)
        | None -> ()

    let run () = program (Cube.finished, Ok Start)

    let test () =
        let draw = CubeShader.cubeShader >> Shader.draw
        let cube = Cube.finished
        //let cube = { cube with yellow = Face (CYellow,(CWhite,CGreen,CRed,CBlue,CYellow,COrange,CBlue,CRed))}
        let cube = { cube with red = Face (CRed,(COrange,CRed,CRed,CRed,CRed,CRed,CWhite,CBlue))}
        draw cube
        let cube =
            cube
            |> Cube.turn Front
            //|> Cube.turn Left
        draw cube
        let cube = cube |> Cube.turn Left
        draw cube
        Console.ReadLine() |> ignore

