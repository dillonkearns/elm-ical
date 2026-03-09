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
        , test "parameter value with embedded quotes is escaped per RFC 6868" <|
            \() ->
                ( "ORGANIZER"
                , Property.CalAddress "test@example.com"
                , [ Property.Parameter ( "CN", "She said \"hello\"" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal "ORGANIZER;CN=She said ^'hello^':mailto:test@example.com"
        , test "parameter value with embedded caret is escaped per RFC 6868" <|
            \() ->
                ( "ORGANIZER"
                , Property.CalAddress "test@example.com"
                , [ Property.Parameter ( "CN", "Test ^ Value" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal "ORGANIZER;CN=Test ^^ Value:mailto:test@example.com"
        , test "parameter value with quotes AND special chars gets both escaping and quoting" <|
            \() ->
                ( "ORGANIZER"
                , Property.CalAddress "test@example.com"
                , [ Property.Parameter ( "CN", "She said \"hi, there\"" ) ]
                )
                    |> Property.encodeProperty
                    |> Expect.equal "ORGANIZER;CN=\"She said ^'hi, there^'\":mailto:test@example.com"
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
