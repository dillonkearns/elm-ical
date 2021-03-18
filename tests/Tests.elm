module Tests exposing (suite)

import Expect
import Iso8601
import Rfc3339
import Test exposing (..)
import Time



--eventGenerate : { a | start : Time.Posix, end : Time.Posix } -> String


eventGenerate details =
    """BEGIN:VEVENT
UID:uid1@example.com
"""
        ++ formatKeys (keys details)
        ++ """
END:VEVENT"""


keys details =
    [ ( "DTSTAMP", details.stamp |> Rfc3339.format )
    , ( "DTSTART", details.start |> Rfc3339.format )
    , ( "DTEND", details.end |> Rfc3339.format )
    , ( "SUMMARY", details.summary )
    ]


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


suite : Test
suite =
    describe "ical event"
        [ test "single event" <|
            \() ->
                { start = toIso8601 "1997-07-14T17:00:00.000Z"
                , end = toIso8601 "1997-07-15T03:59:59.000Z"
                , stamp = toIso8601 "1997-07-14T17:00:00.000Z"
                , summary = "Bastille Day Party"
                }
                    |> eventGenerate
                    |> Expect.equal """BEGIN:VEVENT
UID:uid1@example.com
DTSTAMP:19970714T170000Z
DTSTART:19970714T170000Z
DTEND:19970715T035959Z
SUMMARY:Bastille Day Party
END:VEVENT"""
        ]


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
