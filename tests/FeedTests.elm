module FeedTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Ical
import Iso8601
import Property exposing (DateOrDateTime)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "ical event"
        [ test "example feed" <|
            \() ->
                [ { id = "1"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601D "2013-10-04T22:39:30.000Z"
                  , created = Nothing
                  , lastModified = Nothing
                  , end = toIso8601D "2013-10-06T23:15:00.000Z"
                  , summary = "repeating by month"
                  , description = Just "repeating by month"
                  , organizer = Nothing
                  , location = Nothing
                  , htmlDescription = Nothing
                  , transparency = Nothing
                  , status = Nothing
                  }
                , { id = "2"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601D "2013-10-04T22:39:30.000Z"
                  , created = Nothing
                  , lastModified = Nothing
                  , end = toIso8601D "2013-10-06T23:15:00.000Z"
                  , summary = "This is the title, it escapes commas"
                  , description = Just "This is the description, it escapes commas"
                  , organizer = Nothing
                  , location = Nothing
                  , htmlDescription = Nothing
                  , transparency = Nothing
                  , status = Nothing
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
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
DTSTAMP:20131004T233453Z
UID:1@incrementalelm.com
SUMMARY:repeating by month
DESCRIPTION:repeating by month
END:VEVENT
BEGIN:VEVENT
DTSTART:20131004T223930Z
DTEND:20131006T231500Z
DTSTAMP:20131004T233453Z
UID:2@incrementalelm.com
SUMMARY:This is the title\\, it escapes commas
DESCRIPTION:This is the description\\, it escapes commas
END:VEVENT
END:VCALENDAR"""
        , test "example feed 2" <|
            \() ->
                [ { id = "123"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601D "2013-10-04T22:39:30.000Z"
                  , end = toIso8601D "2013-10-04T23:15:00.000Z"
                  , created = toIso8601 "2013-10-04T23:34:53.000Z" |> Just
                  , lastModified = toIso8601 "2013-10-04T23:34:53.000Z" |> Just
                  , summary = "Simple Event"
                  , description = Nothing
                  , organizer = Nothing
                  , location = Nothing
                  , htmlDescription = Nothing
                  , transparency = Nothing
                  , status = Nothing
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
DTSTART:20131004T223930Z
DTEND:20131004T231500Z
DTSTAMP:20131004T233453Z
UID:123@sebbo.net
SUMMARY:Simple Event
CREATED:20131004T233453Z
LAST-MODIFIED:20131004T233453Z
END:VEVENT
END:VCALENDAR"""
        , test "generate_02" <|
            -- source: https://github.com/sebbo2002/ical-generator/blob/634389543bb057b8767bff6edb562affe16809f0/test/cases.ts#L31
            \() ->
                [ { id = "123"
                  , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                  , start = toIso8601D "2013-10-04T22:39:30.000Z"
                  , end = toIso8601D "2013-10-04T23:15:00.000Z"
                  , created = Nothing
                  , lastModified = Nothing
                  , summary = "Sample Event"
                  , description = Just "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop"
                  , organizer = Nothing
                  , location = Just "localhost"
                  , htmlDescription = Just "<p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop</p>"
                  , transparency = Nothing
                  , status = Nothing
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
DTSTART:20131004T223930Z
DTEND:20131004T231500Z
DTSTAMP:20131004T233453Z
UID:123@sebbo.net
SUMMARY:Sample Event
LOCATION:localhost
DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed
  diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam era
 t\\, sed diam voluptua.\\nbeep boop
X-ALT-DESC;FMTTYPE=text/html:<p>Lorem ipsum dolor sit amet\\, consetetur sa
 dipscing elitr\\, sed diam nonumy eirmod tempor invidunt ut labore et dolor
 e magna aliquyam erat\\, sed diam voluptua.\\nbeep boop</p>
END:VEVENT
END:VCALENDAR"""
        , test "example 1" <|
            -- source: https://github.com/sebbo2002/ical-generator/blob/634389543bb057b8767bff6edb562affe16809f0/test/cases.ts#L31
            \() ->
                { id = "4ot852po37bvri1natdlv4cf6r"
                , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                , start =
                    Property.Date <|
                        Date.fromCalendarDate 2021 Time.Mar 18
                , end =
                    Property.Date <| Date.fromCalendarDate 2021 Time.Mar 19
                , created = toIso8601 "2021-03-18T14:59:37.000Z" |> Just
                , lastModified = toIso8601 "2021-03-18T14:59:37.000Z" |> Just
                , summary = "All day event"
                , description = Just ""
                , organizer = Nothing
                , location = Just ""
                , htmlDescription = Nothing
                , transparency = Just Ical.Transparent
                , status = Just Ical.Confirmed
                }
                    |> Ical.eventGenerate
                        { id = "//sebbo.net//ical-generator.tests//EN"
                        , domain = "incrementalelm.com"
                        , name = Nothing
                        , description = Nothing
                        , url = Nothing
                        }
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20210318
DTEND;VALUE=DATE:20210319
DTSTAMP:20210318T162044Z
UID:4ot852po37bvri1natdlv4cf6r@incrementalelm.com
SUMMARY:All day event
CREATED:20210318T145937Z
LAST-MODIFIED:20210318T145937Z
LOCATION:
DESCRIPTION:
STATUS:CONFIRMED
TRANSP:TRANSPARENT
END:VEVENT"""
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


toIso8601D : String -> DateOrDateTime
toIso8601D string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed |> Property.DateWithTime

        Err error ->
            Debug.todo (Debug.toString error)
