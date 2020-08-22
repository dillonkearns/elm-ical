module Property exposing (..)

import Format
import Rfc3339
import Time


type alias Pair =
    ( String, ValueData )


type PropertyValue
    = SinglePart ValueData
    | MultiPart ( Pair, List Pair )


{-| <https://tools.ietf.org/html/rfc5545#section-3.2.20>
-}
type ValueData
    = Text String
    | CalAddress String -- https://tools.ietf.org/html/rfc5545#section-3.3.3
    | DateTime Time.Posix -- https://tools.ietf.org/html/rfc5545#section-3.3.5


{-| <https://tools.ietf.org/html/rfc5545#section-3.2>
-}
type Parameter
    = Parameter ( String, String )


encodeProperty : ( String, ValueData, List Parameter ) -> String
encodeProperty ( key, value, parameters ) =
    let
        separator =
            if List.isEmpty parameters then
                ":"

            else
                ";"
    in
    key
        ++ separator
        ++ encodeValue value parameters



--encodePair : Pair -> String
--encodePair ( key, value ) =
--    key
--        ++ "="
--        ++ --Format.formatValue
--           encodeValue value


encodeValue : ValueData -> List Parameter -> String
encodeValue data parameters =
    let
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

                CalAddress address ->
                    "mailto:"
                        ++ Format.formatValue address

                DateTime posix ->
                    Rfc3339.format posix
           )


encodeParameter (Parameter ( key, value )) =
    key
        ++ "="
        ++ -- TODO quote value if needed
           quoted value


quoted : String -> String
quoted string =
    -- Property parameter values that contain the COLON, SEMICOLON, or COMMA
    --   character separators MUST be specified as quoted-string text values.
    -- (https://tools.ietf.org/html/rfc5545#section-3.2)
    let
        needsQuotes =
            (string |> String.contains ":") || (string |> String.contains ";") || (string |> String.contains ",")
    in
    if needsQuotes then
        "\"" ++ string ++ "\""

    else
        string
