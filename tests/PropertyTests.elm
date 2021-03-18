module PropertyTests exposing (suite)

import Expect
import Iso8601
import Property
import Test exposing (..)
import Time


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
                    |> Expect.equal """ORGANIZER;CN=Dillon Kearns:mailto:dillon@incrementalelm.com"""
        , test "escape datetime with parameter" <|
            \() ->
                -- example from https://tools.ietf.org/html/rfc5545#section-3.3.5
                ( "DTSTART"
                , Property.DateTime (toIso8601 "1997-07-14T13:30:00.000Z")
                , [ Property.Parameter ( "TZID", "America/New_York" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal """DTSTART;TZID=America/New_York:19970714T133000Z"""
        ]


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
