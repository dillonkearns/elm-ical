module FormatTests exposing (..)

import Expect exposing (Expectation)
import Iso8601
import List.Extra
import Regex
import Test exposing (..)
import Time


formatKeys nodes =
    nodes
        |> List.map
            (\( key, value ) ->
                String.concat
                    [ key
                    , ":"
                    , value
                    ]
            )
        |> String.join "\n"


normalizeField : ( String, String ) -> String
normalizeField ( key, value ) =
    String.concat
        [ key
        , ":"
        , formatValue value
        ]
        |> splitOverflowingLines


formatValue : String -> String
formatValue value =
    value
        |> Regex.replace (reg "[\\\\;,\"]") (\{ match } -> "\\" ++ match)
        |> Regex.replace (reg "(?:\u{000D}\n|\u{000D}|\n)") (\_ -> "\\n")


splitOverflowingLines : String -> String
splitOverflowingLines string =
    string
        |> String.toList
        |> List.Extra.greedyGroupsOf 74
        |> List.map String.fromList
        |> String.join "\n "


reg string =
    Regex.fromString string
        |> Maybe.withDefault Regex.never


suite : Test
suite =
    describe "ical event"
        [ test "single event" <|
            \() ->
                ( "DESCRIPTION", "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop" )
                    |> normalizeField
                    |> Expect.equal """DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed
  diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam era
 t\\, sed diam voluptua.\\nbeep boop"""
        ]


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
