module ContentLine exposing (ContentLine, parse, unfold)

{-| Line unfolding and content line parsing per RFC 5545.
-}


{-| A parsed content line with name, parameters, and value.
-}
type alias ContentLine =
    { name : String
    , parameters : List ( String, String )
    , value : String
    }


{-| Unfold lines per RFC 5545 Section 3.1: CRLF followed by a space or tab
is a line continuation. We also handle LF-only continuation for robustness.
-}
unfold : String -> String
unfold input =
    input
        |> String.replace "\u{000D}\n " ""
        |> String.replace "\u{000D}\n\t" ""
        |> String.replace "\n " ""
        |> String.replace "\n\t" ""


{-| Parse a single content line into name, parameters, and value.
Format: NAME;PARAM=VALUE;PARAM=VALUE:value
-}
parse : String -> Result String ContentLine
parse line =
    case splitNameFromRest line of
        Nothing ->
            Err ("Invalid content line: " ++ line)

        Just ( name, rest ) ->
            let
                ( parameters, value ) =
                    parseParamsAndValue rest
            in
            Ok
                { name = String.toUpper name
                , parameters = parameters
                , value = value
                }


splitNameFromRest : String -> Maybe ( String, String )
splitNameFromRest line =
    let
        nameChars : String -> Int -> Maybe Int
        nameChars s i =
            case String.uncons (String.dropLeft i s) of
                Nothing ->
                    Nothing

                Just ( c, _ ) ->
                    if c == ':' || c == ';' then
                        Just i

                    else if Char.isAlphaNum c || c == '-' || c == '_' then
                        nameChars s (i + 1)

                    else
                        Nothing
    in
    case nameChars line 0 of
        Nothing ->
            Nothing

        Just 0 ->
            Nothing

        Just i ->
            Just ( String.left i line, String.dropLeft i line )


parseParamsAndValue : String -> ( List ( String, String ), String )
parseParamsAndValue rest =
    case String.uncons rest of
        Just ( ':', value ) ->
            ( [], value )

        Just ( ';', afterSemicolon ) ->
            parseParameters [] afterSemicolon

        _ ->
            ( [], rest )


parseParameters : List ( String, String ) -> String -> ( List ( String, String ), String )
parseParameters acc input =
    case parseOneParameter input of
        Nothing ->
            ( List.reverse acc, input )

        Just ( param, remaining ) ->
            case String.uncons remaining of
                Just ( ';', afterSemicolon ) ->
                    parseParameters (param :: acc) afterSemicolon

                Just ( ':', value ) ->
                    ( List.reverse (param :: acc), value )

                _ ->
                    ( List.reverse (param :: acc), remaining )


parseOneParameter : String -> Maybe ( ( String, String ), String )
parseOneParameter input =
    case findChar '=' input of
        Nothing ->
            Nothing

        Just eqIndex ->
            let
                paramName : String
                paramName =
                    String.left eqIndex input

                afterEq : String
                afterEq =
                    String.dropLeft (eqIndex + 1) input
            in
            case String.uncons afterEq of
                Just ( '"', afterQuote ) ->
                    case findChar '"' afterQuote of
                        Nothing ->
                            Just ( ( paramName, afterQuote ), "" )

                        Just closeIndex ->
                            let
                                paramValue : String
                                paramValue =
                                    String.left closeIndex afterQuote

                                remaining : String
                                remaining =
                                    String.dropLeft (closeIndex + 1) afterQuote
                            in
                            Just ( ( paramName, paramValue ), remaining )

                _ ->
                    let
                        ( paramValue, remaining ) =
                            takeUntilParamEnd afterEq
                    in
                    Just ( ( paramName, paramValue ), remaining )


takeUntilParamEnd : String -> ( String, String )
takeUntilParamEnd input =
    takeUntilParamEndHelp 0 input


takeUntilParamEndHelp : Int -> String -> ( String, String )
takeUntilParamEndHelp i input =
    case String.uncons (String.dropLeft i input) of
        Nothing ->
            ( input, "" )

        Just ( c, _ ) ->
            if c == ';' || c == ':' then
                ( String.left i input, String.dropLeft i input )

            else
                takeUntilParamEndHelp (i + 1) input


findChar : Char -> String -> Maybe Int
findChar target input =
    findCharHelp target 0 input


findCharHelp : Char -> Int -> String -> Maybe Int
findCharHelp target i input =
    case String.uncons (String.dropLeft i input) of
        Nothing ->
            Nothing

        Just ( c, _ ) ->
            if c == target then
                Just i

            else
                findCharHelp target (i + 1) input
