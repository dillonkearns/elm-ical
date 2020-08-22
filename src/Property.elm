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
    | CalAddress String (List Parameter) -- https://tools.ietf.org/html/rfc5545#section-3.3.3
    | DateTime Time.Posix -- https://tools.ietf.org/html/rfc5545#section-3.3.5


{-| <https://tools.ietf.org/html/rfc5545#section-3.2>
-}
type Parameter
    = Parameter ( String, String )


encode : ( String, ValueData ) -> String
encode ( key, value ) =
    --case value of
    --    SinglePart singleValue ->
    --        key ++ ":" ++ encodeValue singleValue
    --
    --    MultiPart ( firstPair, remainingPairs ) ->
    let
        separator =
            if isMultipart value then
                ";"

            else
                ":"
    in
    key
        ++ separator
        ++ (encodeValue value
            --|> List.map encodePair
            --|> String.join ";"
            --|> encodeValue
           )


encodePair : Pair -> String
encodePair ( key, value ) =
    key
        ++ "="
        ++ --Format.formatValue
           encodeValue value


isMultipart : ValueData -> Bool
isMultipart data =
    case data of
        Text string ->
            False

        CalAddress string parameters ->
            not (List.isEmpty parameters)

        DateTime posix ->
            False


encodeValue : ValueData -> String
encodeValue data =
    case data of
        Text text ->
            Format.formatValue text

        CalAddress address parameters ->
            case parameters of
                [] ->
                    "mailto:" ++ Format.formatValue address

                _ ->
                    (parameters
                        |> List.map encodeParameter
                        |> String.join ";"
                    )
                        ++ ":mailto:"
                        ++ Format.formatValue address

        DateTime posix ->
            Rfc3339.format posix


encodeParameter (Parameter ( key, value )) =
    key
        ++ "="
        ++ -- TODO quote value if needed (https://tools.ietf.org/html/rfc5545#section-3.2)
           -- Property parameter values that contain the COLON, SEMICOLON, or COMMA
           --   character separators MUST be specified as quoted-string text values.
           quoted value


quoted : String -> String
quoted string =
    "\"" ++ string ++ "\""
