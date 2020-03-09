module FormatTests exposing (..)

import Expect exposing (Expectation)
import Iso8601
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
    """DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed
  diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam era
 t\\, sed diam voluptua.\\nbeep boop"""


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
