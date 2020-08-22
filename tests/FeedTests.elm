module FeedTests exposing (..)

import Expect exposing (Expectation)
import Format
import Iso8601
import Rfc3339
import Test exposing (..)
import Time


type alias Event =
    { stamp : Time.Posix
    , start : Time.Posix
    , end : Time.Posix
    , summary : String
    , id : String
    }


eventGenerate : Config -> Event -> String
eventGenerate config details =
    """BEGIN:VEVENT
"""
        ++ formatKeys (keys config details)
        ++ """
END:VEVENT"""


keys : Config -> Event -> List ( String, String )
keys config details =
    [ ( "UID", details.id ++ "@" ++ config.domain )
    , ( "DTSTAMP", details.stamp |> Rfc3339.format ) -- https://www.kanzaki.com/docs/ical/dtstamp.html
    , ( "DTSTART", details.start |> Rfc3339.format )
    , ( "DTEND", details.end |> Rfc3339.format )
    , ( "SUMMARY", details.summary )
    ]


formatKeys : List ( String, String ) -> String
formatKeys nodes =
    nodes
        |> List.map Format.normalizeField
        |> String.join "\n"


type alias Config =
    { id : String, domain : String }


generateFeed : Config -> List Event -> String
generateFeed config events =
    """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//incrementalelm.com//elm-ical.tests//EN
""" ++ String.join "\n" (List.map (eventGenerate config) events) ++ """
END:VCALENDAR"""


suite : Test
suite =
    describe "ical event"
        [ test "example feed" <|
            \() ->
                [ { id = "1"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601 "2013-10-04T22:39:30.000Z"
                  , end = toIso8601 "2013-10-06T23:15:00.000Z"
                  , summary = "repeating by month"
                  }
                , { id = "2"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601 "2013-10-04T22:39:30.000Z"
                  , end = toIso8601 "2013-10-06T23:15:00.000Z"
                  , summary = "repeating by day, twice"
                  }
                ]
                    |> generateFeed { id = "//incrementalelm.com//elm-ical.tests//EN", domain = "incrementalelm.com" }
                    |> expectEqualLines """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//incrementalelm.com//elm-ical.tests//EN
BEGIN:VEVENT
UID:1@incrementalelm.com
DTSTAMP:20131004T233453Z
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
SUMMARY:repeating by month
END:VEVENT
BEGIN:VEVENT
UID:2@incrementalelm.com
DTSTAMP:20131004T233453Z
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
SUMMARY:repeating by day\\, twice
END:VEVENT
END:VCALENDAR"""
        ]


expectEqualLines : String -> String -> Expectation
expectEqualLines actual expected =
    Expect.equalLists (String.lines actual) (String.lines expected)


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
