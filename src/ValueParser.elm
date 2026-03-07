module ValueParser exposing (DateTimeParts, parseDate, parseDateTime, unescapeText)

{-| Parsers for iCal value types: DATE, DATE-TIME, and TEXT unescaping.
-}


{-| Intermediate parsed date-time parts. The caller decides whether to wrap
this as UTC, local, or with-TZID based on context.
-}
type alias DateTimeParts =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , isUtc : Bool
    }


{-| Parse an iCal DATE value like "20210318" into year/month/day.
-}
parseDate : String -> Result String { year : Int, month : Int, day : Int }
parseDate input =
    if String.length input /= 8 then
        Err ("Invalid DATE: " ++ input)

    else
        case
            ( String.toInt (String.left 4 input)
            , String.toInt (String.slice 4 6 input)
            , String.toInt (String.slice 6 8 input)
            )
        of
            ( Just year, Just month, Just day ) ->
                Ok { year = year, month = month, day = day }

            _ ->
                Err ("Invalid DATE: " ++ input)


{-| Parse an iCal DATE-TIME value like "20210318T162044Z" or "20210318T162044".
Returns intermediate parts with an isUtc flag.
-}
parseDateTime : String -> Result String DateTimeParts
parseDateTime input =
    let
        hasZ : Bool
        hasZ =
            String.endsWith "Z" input

        base : String
        base =
            if hasZ then
                String.dropRight 1 input

            else
                input
    in
    if String.length base /= 15 then
        Err ("Invalid DATE-TIME: " ++ input)

    else
        case
            ( String.toInt (String.left 4 base)
            , String.toInt (String.slice 4 6 base)
            , String.toInt (String.slice 6 8 base)
            )
        of
            ( Just year, Just month, Just day ) ->
                if String.slice 8 9 base /= "T" then
                    Err ("Invalid DATE-TIME: " ++ input)

                else
                    case
                        ( String.toInt (String.slice 9 11 base)
                        , String.toInt (String.slice 11 13 base)
                        , String.toInt (String.slice 13 15 base)
                        )
                    of
                        ( Just hour, Just minute, Just second ) ->
                            Ok
                                { year = year
                                , month = month
                                , day = day
                                , hour = hour
                                , minute = minute
                                , second = second
                                , isUtc = hasZ
                                }

                        _ ->
                            Err ("Invalid DATE-TIME: " ++ input)

            _ ->
                Err ("Invalid DATE-TIME: " ++ input)


{-| Unescape iCal TEXT values per RFC 5545 Section 3.3.11.
-}
unescapeText : String -> String
unescapeText input =
    unescapeHelp (String.toList input) []
        |> List.reverse
        |> String.fromList


unescapeHelp : List Char -> List Char -> List Char
unescapeHelp remaining acc =
    case remaining of
        [] ->
            acc

        '\\' :: 'n' :: rest ->
            unescapeHelp rest ('\n' :: acc)

        '\\' :: 'N' :: rest ->
            unescapeHelp rest ('\n' :: acc)

        '\\' :: '\\' :: rest ->
            unescapeHelp rest ('\\' :: acc)

        '\\' :: ',' :: rest ->
            unescapeHelp rest (',' :: acc)

        '\\' :: ';' :: rest ->
            unescapeHelp rest (';' :: acc)

        '\\' :: c :: rest ->
            unescapeHelp rest (c :: acc)

        c :: rest ->
            unescapeHelp rest (c :: acc)
