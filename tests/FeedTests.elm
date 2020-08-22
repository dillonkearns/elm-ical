module FeedTests exposing (..)

import Expect exposing (Expectation)
import Ical
import Iso8601
import Test exposing (..)
import Time


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
                  , organizer = Nothing
                  }
                , { id = "2"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601 "2013-10-04T22:39:30.000Z"
                  , end = toIso8601 "2013-10-06T23:15:00.000Z"
                  , summary = "repeating by day, twice"
                  , organizer = Nothing
                  }
                ]
                    |> Ical.generate { id = "//incrementalelm.com//elm-ical.tests//EN", domain = "incrementalelm.com" }
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
