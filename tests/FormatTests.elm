module FormatTests exposing (suite)

import Expect
import Format
import Test exposing (..)


suite : Test
suite =
    describe "ical event"
        [ test "single event" <|
            \() ->
                ( "DESCRIPTION", "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop" )
                    |> Format.normalizeField
                    |> Expect.equal """DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed
  diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam era
 t\\, sed diam voluptua.\\nbeep boop"""
        ]
