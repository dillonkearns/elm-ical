module FeedTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Ical
import Ical.Recurrence as Recurrence
import Iso8601
import Test exposing (..)
import Time


suite : Test
suite =
    describe "ical event"
        [ test "example feed" <|
            \() ->
                [ Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "repeating by month"
                    }
                    |> Ical.withDescription "repeating by month"
                , Ical.event
                    { id = "2"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "This is the title, it escapes commas"
                    }
                    |> Ical.withDescription "This is the description, it escapes commas"
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//incrementalelm.com//elm-ical.tests//EN"
                            , domain = "incrementalelm.com"
                            }
                            |> Ical.withName "Incremental Elm Live"
                            |> Ical.withCalendarDescription "Pairing on Elm Open Source and learning from the community."
                            |> Ical.withUrl "https://incrementalelm.com/live.ics"
                        )
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
                [ Ical.event
                    { id = "123"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-04T23:15:00.000Z"
                            }
                    , summary = "Simple Event"
                    }
                    |> Ical.withCreated (toIso8601 "2013-10-04T23:34:53.000Z")
                    |> Ical.withLastModified (toIso8601 "2013-10-04T23:34:53.000Z")
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//sebbo.net//ical-generator.tests//EN"
                            , domain = "sebbo.net"
                            }
                        )
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
                [ Ical.event
                    { id = "123"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-04T23:15:00.000Z"
                            }
                    , summary = "Sample Event"
                    }
                    |> Ical.withDescription "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop"
                    |> Ical.withLocation "localhost"
                    |> Ical.withHtmlDescription "<p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop</p>"
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//sebbo.net//ical-generator.tests//EN"
                            , domain = "sebbo.net"
                            }
                        )
                    |> expectEqualLines
                        (String.join "\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//sebbo.net//ical-generator.tests//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20131004T223930Z"
                            , "DTEND:20131004T231500Z"
                            , "DTSTAMP:20131004T233453Z"
                            , "UID:123@sebbo.net"
                            , "SUMMARY:Sample Event"
                            , "LOCATION:localhost"
                            , "DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed "
                            , " diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"
                            , " \\, sed diam voluptua.\\nbeep boop"
                            , "X-ALT-DESC;FMTTYPE=text/html:<p>Lorem ipsum dolor sit amet\\, consetetur sad"
                            , " ipscing elitr\\, sed diam nonumy eirmod tempor invidunt ut labore et dolore"
                            , "  magna aliquyam erat\\, sed diam voluptua.\\nbeep boop</p>"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            ]
                        )
        , test "all-day event" <|
            \() ->
                Ical.event
                    { id = "4ot852po37bvri1natdlv4cf6r"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.allDay (Date.fromCalendarDate 2021 Time.Mar 18)
                    , summary = "All day event"
                    }
                    |> Ical.withCreated (toIso8601 "2021-03-18T14:59:37.000Z")
                    |> Ical.withLastModified (toIso8601 "2021-03-18T14:59:37.000Z")
                    |> Ical.withLocation ""
                    |> Ical.withDescription ""
                    |> Ical.withTransparency Ical.Transparent
                    |> Ical.withStatus Ical.Confirmed
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//sebbo.net//ical-generator.tests//EN"
                            , domain = "incrementalelm.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20210318
DTEND;VALUE=DATE:20210319
DTSTAMP:20210318T162044Z
UID:4ot852po37bvri1natdlv4cf6r@incrementalelm.com
SUMMARY:All day event
CREATED:20210318T145937Z
LAST-MODIFIED:20210318T145937Z
STATUS:CONFIRMED
TRANSP:TRANSPARENT
END:VEVENT"""
        , test "output uses CRLF line endings" <|
            \() ->
                [ Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "test"
                    }
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> String.contains "\u{000D}\n"
                    |> Expect.equal True
        , test "double quotes are not escaped in text" <|
            \() ->
                [ Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "She said \"hello\""
                    }
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> String.contains "She said \"hello\""
                    |> Expect.equal True
        , test "output ends with CRLF" <|
            \() ->
                [ Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "test"
                    }
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> String.endsWith "\u{000D}\n"
                    |> Expect.equal True
        , test "empty optional fields are not emitted" <|
            \() ->
                let
                    output : String
                    output =
                        Ical.event
                            { id = "1"
                            , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                            , time =
                                Ical.withTime
                                    { start = toIso8601 "2013-10-04T22:39:30.000Z"
                                    , end = toIso8601 "2013-10-06T23:15:00.000Z"
                                    }
                            , summary = "test"
                            }
                            |> Ical.withDescription ""
                            |> Ical.withLocation ""
                            |> Ical.generateEvent
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                Expect.all
                    [ \o -> o |> String.contains "DESCRIPTION:" |> Expect.equal False
                    , \o -> o |> String.contains "LOCATION:" |> Expect.equal False
                    ]
                    output
        , test "CalAddress values are not text-escaped" <|
            \() ->
                Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "test"
                    }
                    |> Ical.withOrganizer { name = "Test", email = "user;tag@example.com" }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> String.contains "mailto:user;tag@example.com"
                    |> Expect.equal True
        , test "URL property is not text-escaped" <|
            \() ->
                [ Ical.event
                    { id = "1"
                    , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2013-10-04T22:39:30.000Z"
                            , end = toIso8601 "2013-10-06T23:15:00.000Z"
                            }
                    , summary = "test"
                    }
                ]
                    |> Ical.generate
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                            |> Ical.withUrl "https://example.com/cal?a=1,2;b=3"
                        )
                    |> String.contains "URL:https://example.com/cal?a=1,2;b=3"
                    |> Expect.equal True
        , test "empty calendar-level fields are not emitted" <|
            \() ->
                let
                    output : String
                    output =
                        [ Ical.event
                            { id = "1"
                            , stamp = toIso8601 "2013-10-04T23:34:53.000Z"
                            , time =
                                Ical.withTime
                                    { start = toIso8601 "2013-10-04T22:39:30.000Z"
                                    , end = toIso8601 "2013-10-06T23:15:00.000Z"
                                    }
                            , summary = "test"
                            }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                    |> Ical.withName ""
                                    |> Ical.withCalendarDescription ""
                                    |> Ical.withUrl ""
                                )
                in
                Expect.all
                    [ \o -> o |> String.contains "NAME:" |> Expect.equal False
                    , \o -> o |> String.contains "DESCRIPTION:" |> Expect.equal False
                    , \o -> o |> String.contains "URL:" |> Expect.equal False
                    ]
                    output
        , test "empty calendar has no blank line before END:VCALENDAR" <|
            \() ->
                Ical.generate
                    (Ical.config
                        { id = "//test//test//EN"
                        , domain = "test.com"
                        }
                    )
                    []
                    |> expectEqualLines """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//test//test//EN
END:VCALENDAR"""
        , test "multi-day all-day event uses inclusive end date" <|
            \() ->
                Ical.event
                    { id = "multi-day"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.allDayRange
                            { start = Date.fromCalendarDate 2021 Time.Mar 18
                            , end = Date.fromCalendarDate 2021 Time.Mar 20
                            }
                    , summary = "3-day event"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20210318
DTEND;VALUE=DATE:20210321
DTSTAMP:20210318T162044Z
UID:multi-day@test.com
SUMMARY:3-day event
END:VEVENT"""
        , test "AllDay with reversed dates normalizes start/end" <|
            \() ->
                Ical.event
                    { id = "reversed"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.allDayRange
                            { start = Date.fromCalendarDate 2021 Time.Mar 20
                            , end = Date.fromCalendarDate 2021 Time.Mar 18
                            }
                    , summary = "Reversed dates"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20210318
DTEND;VALUE=DATE:20210321
DTSTAMP:20210318T162044Z
UID:reversed@test.com
SUMMARY:Reversed dates
END:VEVENT"""
        , test "WithTime with reversed times normalizes start/end" <|
            \() ->
                Ical.event
                    { id = "reversed-time"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T11:00:00.000Z"
                            , end = toIso8601 "2021-03-18T10:00:00.000Z"
                            }
                    , summary = "Reversed times"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T110000Z
DTSTAMP:20210318T162044Z
UID:reversed-time@test.com
SUMMARY:Reversed times
END:VEVENT"""
        , test "generate event with RRULE" <|
            \() ->
                Ical.event
                    { id = "recurring-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Weekly standup"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                            |> Ical.withByDay [ Recurrence.Every Time.Mon, Recurrence.Every Time.Wed, Recurrence.Every Time.Fri ]
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T110000Z
DTSTAMP:20210318T162044Z
UID:recurring-1@test.com
SUMMARY:Weekly standup
RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR
END:VEVENT"""
        , test "generate RRULE with COUNT" <|
            \() ->
                Ical.event
                    { id = "count-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Limited series"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Daily { every = 2 })
                            |> Ical.withCount 10
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T110000Z
DTSTAMP:20210318T162044Z
UID:count-1@test.com
SUMMARY:Limited series
RRULE:FREQ=DAILY;INTERVAL=2;COUNT=10
END:VEVENT"""
        , test "negative interval clamps to 1" <|
            \() ->
                Ical.event
                    { id = "clamped"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Clamped"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Daily { every = -3 })
                            |> Ical.withCount -5
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T110000Z
DTSTAMP:20210318T162044Z
UID:clamped@test.com
SUMMARY:Clamped
RRULE:FREQ=DAILY;COUNT=1
END:VEVENT"""
        , test "generate RRULE with BYMONTH using Time.Month" <|
            \() ->
                Ical.event
                    { id = "bymonth-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Quarterly"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Yearly { every = 1 })
                            |> Ical.withByMonth [ Time.Jan, Time.Apr, Time.Jul, Time.Oct ]
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T110000Z
DTSTAMP:20210318T162044Z
UID:bymonth-1@test.com
SUMMARY:Quarterly
RRULE:FREQ=YEARLY;BYMONTH=1,4,7,10
END:VEVENT"""
        , test "generate event with ATTENDEE" <|
            \() ->
                Ical.event
                    { id = "meeting-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Team meeting"
                    }
                    |> Ical.withAttendee { email = "jane@example.com", name = "Jane Smith" }
                    |> Ical.withAttendee { email = "bob@example.com", name = "Bob Jones" }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> (\output ->
                            Expect.all
                                [ \o -> o |> String.contains "ATTENDEE;CN=Jane Smith:mailto:jane@example.com" |> Expect.equal True
                                , \o -> o |> String.contains "ATTENDEE;CN=Bob Jones:mailto:bob@example.com" |> Expect.equal True
                                ]
                                output
                       )
        ]


expectEqualLines : String -> String -> Expectation
expectEqualLines expected actual =
    Expect.equalLists (String.split "\n" expected) (dropTrailingEmpty (String.split "\u{000D}\n" actual))


dropTrailingEmpty : List String -> List String
dropTrailingEmpty lines =
    case List.reverse lines of
        "" :: rest ->
            List.reverse rest

        _ ->
            lines


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
