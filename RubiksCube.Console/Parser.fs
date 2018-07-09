namespace RubiksCube.Console

module Parser =

    type Parser<'A> = private | Parser of (string -> ('A * string) seq)

    let char (c: char): Parser<char> =
        fun (input: string) ->
            if input.Length = 0 then
                Seq.empty
            else
                if input.[0] = c then
                    (c, input.Substring 1)
                    |> Seq.singleton
                else
                    Seq.empty
        |> Parser

    let token (s: string): Parser<string> =
        fun (input: string) ->
            if input.Length >= s.Length && input.Substring (0, s.Length) = s then
                (s, input.Substring s.Length)
                |> Seq.singleton
            else
                Seq.empty
        |> Parser

    let (<|>) (Parser p1) (Parser p2) =
        fun (input: string) ->
            Seq.append (p1 input) (p2 input)
        |> Parser

    let empty: Parser<unit> =
        fun (input: string) ->
            ((), input)
            |> Seq.singleton
        |> Parser

    let constant x =
        fun (s: string) ->
            (x, s)
            |> Seq.singleton
        |> Parser

    let (<%>) f (Parser p) =
        fun (input: string) ->
            p input
            |> Seq.map (fun (value,rest) -> f value, rest)
        |> Parser

    let fmap = (<%>)

    let (<*>) (Parser f: Parser<'A -> 'B>) (Parser x: Parser<'A>): Parser<'B> =
        fun (input: string) ->
            f input
            |> Seq.collect (fun (f,rest) ->
                x rest
                |> Seq.map (fun (x,rest') -> f x, rest')
            )
        |> Parser

    let any (Parser p: Parser<'A>): Parser<'A list>=
        let rec parse (input: string): ('A list * string) seq =
            let result = p input
            result
            |> Seq.isEmpty
            |> function
                | true -> ([], input) |> Seq.singleton
                | false ->
                    result
                    |> Seq.collect (fun (value,rest) ->
                        let subres = parse rest
                        subres
                        |> Seq.map (fun (values,rest) -> (value :: values, rest))
                    )
        Parser parse

    let some (p: Parser<'A>): Parser<'A list> =
       (fun h t -> h :: t) <%> p <*> any p

    let ( *> ) (Parser l) (Parser r) =
        fun (input: string) ->
            l input
            |> Seq.collect (fun (_, rest) ->
                r rest
            )
        |> Parser

    let ( <* ) (Parser l) (Parser r) =
        fun (input: string) ->
            l input
            |> Seq.collect (fun (x, rest) ->
                r rest
                |> Seq.map (fun (_,rest) -> x, rest)
            )
        |> Parser

    open System

    let whitespace: Parser<string> =
        (List.toArray<char> >> String) <%> any (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

    let runParser (Parser p) (input: string) =
        p input
        |> Seq.tryFind (fun (_,rest) -> rest = "")
        |> Option.map (fun (x,_) -> x)