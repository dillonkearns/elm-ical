module FeedTests exposing (suite)

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
                  , created = Nothing
                  , lastModified = Nothing
                  , end = toIso8601 "2013-10-06T23:15:00.000Z"
                  , summary = "repeating by month"
                  , description = Just "repeating by month"
                  , organizer = Nothing
                  }
                , { id = "2"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601 "2013-10-04T22:39:30.000Z"
                  , created = Nothing
                  , lastModified = Nothing
                  , end = toIso8601 "2013-10-06T23:15:00.000Z"
                  , summary = "This is the title, it escapes commas"
                  , description = Just "This is the description, it escapes commas"
                  , organizer = Nothing
                  }
                ]
                    |> Ical.generate
                        { id = "//incrementalelm.com//elm-ical.tests//EN"
                        , domain = "incrementalelm.com"
                        , name = Just "Incremental Elm Live"
                        , description = Just "Pairing on Elm Open Source and learning from the community."
                        , url = Just "https://incrementalelm.com/live.ics"
                        }
                    |> expectEqualLines """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//incrementalelm.com//elm-ical.tests//EN
NAME:Incremental Elm Live
DESCRIPTION:Pairing on Elm Open Source and learning from the community.
URL:https://incrementalelm.com/live.ics
BEGIN:VEVENT
UID:1@incrementalelm.com
DTSTAMP:20131004T233453Z
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
SUMMARY:repeating by month
DESCRIPTION:repeating by month
END:VEVENT
BEGIN:VEVENT
UID:2@incrementalelm.com
DTSTAMP:20131004T233453Z
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
SUMMARY:This is the title\\, it escapes commas
DESCRIPTION:This is the description\\, it escapes commas
END:VEVENT
END:VCALENDAR"""
        , test "example feed 2" <|
            \() ->
                [ { id = "123"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601 "2013-10-04T22:39:30.000Z"
                  , end = toIso8601 "2013-10-04T23:15:00.000Z"
                  , created = toIso8601 "2013-10-04T23:34:53.000Z" |> Just
                  , lastModified = toIso8601 "2013-10-04T23:34:53.000Z" |> Just
                  , summary = "Simple Event"
                  , description = Nothing
                  , organizer = Nothing
                  }
                ]
                    |> Ical.generate
                        { id = "//sebbo.net//ical-generator.tests//EN"
                        , domain = "sebbo.net"
                        , name = Nothing
                        , description = Nothing
                        , url = Nothing
                        }
                    |> expectEqualLines """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//sebbo.net//ical-generator.tests//EN
BEGIN:VEVENT
UID:123@sebbo.net
DTSTAMP:20131004T233453Z
DTSTART:20131004T223930Z
DTEND:20131004T231500Z
SUMMARY:Simple Event
CREATED:20131004T233453Z
LAST-MODIFIED:20131004T233453Z
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
