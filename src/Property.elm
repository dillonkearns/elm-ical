module Property exposing (Parameter(..), ValueData(..), encodeProperty)

import Date exposing (Date)
import Format
import IcalDateTime
import Time


{-| <https://tools.ietf.org/html/rfc5545#section-3.2.20>
-}
type ValueData
    = Text String
    | Uri String
    | CalAddress String
    | DateTime Time.Posix
    | DateValue Date


{-| <https://tools.ietf.org/html/rfc5545#section-3.2>
-}
type Parameter
    = Parameter ( String, String )


encodeProperty : ( String, ValueData, List Parameter ) -> String
encodeProperty ( key, value, parameters ) =
    let
        separator : String
        separator =
            if List.isEmpty parameters then
                ":"

            else
                ";"
    in
    (key
        ++ separator
        ++ encodeValue value parameters
    )
        |> Format.splitOverflowingLines


encodeValue : ValueData -> List Parameter -> String
encodeValue data parameters =
    let
        paramPrefix : String
        paramPrefix =
            case parameters of
                [] ->
                    ""

                _ ->
                    (parameters
                        |> List.map encodeParameter
                        |> String.join ";"
                    )
                        ++ ":"
    in
    paramPrefix
        ++ (case data of
                Text text ->
                    Format.formatValue text

                Uri uri ->
                    uri

                CalAddress address ->
                    "mailto:" ++ address

                DateTime posix ->
                    IcalDateTime.format posix

                DateValue date ->
                    IcalDateTime.formatDate date
           )


encodeParameter : Parameter -> String
encodeParameter (Parameter ( key, value )) =
    key
        ++ "="
        ++ quoted value


quoted : String -> String
quoted string =
    let
        needsQuotes : Bool
        needsQuotes =
            (string |> String.contains ":") || (string |> String.contains ";") || (string |> String.contains ",")
    in
    if needsQuotes then
        "\"" ++ string ++ "\""

    else
        string
