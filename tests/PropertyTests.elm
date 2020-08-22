module PropertyTests exposing (..)

import Expect exposing (Expectation)
import Format
import Property
import Test exposing (..)


suite : Test
suite =
    describe "property"
        [ test "organizer is escaped correctly" <|
            \() ->
                ( "ORGANIZER"
                , Property.CalAddress "dillon@incrementalelm.com"
                , [ Property.Parameter ( "CN", "Dillon Kearns" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal """ORGANIZER;CN="Dillon Kearns":mailto:dillon@incrementalelm.com"""
        , test "another thing is escaped correctly" <|
            \() ->
                ( "ORGANIZER"
                , Property.CalAddress "dillon@incrementalelm.com"
                , [ Property.Parameter ( "CN", "Dillon Kearns" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal """ORGANIZER;CN="Dillon Kearns":mailto:dillon@incrementalelm.com"""
        ]
