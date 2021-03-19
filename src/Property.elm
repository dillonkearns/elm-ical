module Property exposing (DateOrDateTime(..), Parameter(..), ValueData(..), encodeProperty)

import Date exposing (Date)
import Format
import Rfc3339
import Time



--type alias Pair =
--    ( String, ValueData )
--{-| https://tools.ietf.org/html/rfc5545#section-3.1.2 -}
--type PropertyValue
--    = SinglePart ValueData
--    | MultiPart ( Pair, List Pair )


{-| <https://tools.ietf.org/html/rfc5545#section-3.2.20>
-}
type ValueData
    = Text String
    | CalAddress String -- https://tools.ietf.org/html/rfc5545#section-3.3.3
    | DateTime Time.Posix -- https://tools.ietf.org/html/rfc5545#section-3.3.5
    | DateOrTime DateOrDateTime -- https://tools.ietf.org/html/rfc5545#section-3.3.5


type DateOrDateTime
    = Date Date
    | DateWithTime Time.Posix


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



--encodePair : Pair -> String
--encodePair ( key, value ) =
--    key
--        ++ "="
--        ++ --Format.formatValue
--           encodeValue value


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

                CalAddress address ->
                    "mailto:"
                        ++ Format.formatValue address

                DateTime posix ->
                    Rfc3339.format posix

                DateOrTime dateOrDateTime ->
                    case dateOrDateTime of
                        DateWithTime posix ->
                            Rfc3339.format posix

                        Date date ->
                            Rfc3339.formatDateISO8601_2004 date
           )


encodeParameter : Parameter -> String
encodeParameter (Parameter ( key, value )) =
    key
        ++ "="
        ++ quoted value


quoted : String -> String
quoted string =
    -- Property parameter values that contain the COLON, SEMICOLON, or COMMA
    --   character separators MUST be specified as quoted-string text values.
    -- (https://tools.ietf.org/html/rfc5545#section-3.2)
    let
        needsQuotes : Bool
        needsQuotes =
            (string |> String.contains ":") || (string |> String.contains ";") || (string |> String.contains ",")
    in
    if needsQuotes then
        "\"" ++ string ++ "\""

    else
        string
