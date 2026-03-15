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
        , test "generate RRULE drops invalid numeric BY* values" <|
            \() ->
                Ical.event
                    { id = "sanitized-rrule"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Sanitized recurrence"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Yearly { every = 1 })
                            |> Ical.withByMonthDay [ 0, 1, 32, -1, -32 ]
                            |> Ical.withBySetPos [ 0, 1, 366, 367, -1, -367 ]
                            |> Ical.withByHour [ -1, 0, 23, 24 ]
                            |> Ical.withByMinute [ -1, 0, 59, 60 ]
                            |> Ical.withBySecond [ -1, 0, 60, 61 ]
                            |> Ical.withByYearDay [ -367, -1, 0, 1, 366, 367 ]
                            |> Ical.withByWeekNo [ -54, -1, 0, 1, 53, 54 ]
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines
                        (String.join "\n"
                            [ "BEGIN:VEVENT"
                            , "DTSTART:20210318T100000Z"
                            , "DTEND:20210318T110000Z"
                            , "DTSTAMP:20210318T162044Z"
                            , "UID:sanitized-rrule@test.com"
                            , "SUMMARY:Sanitized recurrence"
                            , "RRULE:FREQ=YEARLY;BYMONTHDAY=1,-1;BYSETPOS=1,366,-1;BYHOUR=0,23;BYMINUTE=0,"
                            , " 59;BYSECOND=0,60;BYYEARDAY=-1,1,366;BYWEEKNO=-1,1,53"
                            , "END:VEVENT"
                            ]
                        )
        , test "generate RRULE drops invalid combinations for weekly all-day events" <|
            \() ->
                Ical.event
                    { id = "weekly-all-day-normalized"
                    , stamp = toIso8601 "2024-01-01T00:00:00.000Z"
                    , time = Ical.allDay (Date.fromCalendarDate 2024 Time.Jan 1)
                    , summary = "Normalized weekly all-day recurrence"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                            |> Ical.withByDay [ Recurrence.Every Time.Mon, Recurrence.Every2nd Time.Tue ]
                            |> Ical.withByMonthDay [ 1, -1 ]
                            |> Ical.withBySetPos [ 1 ]
                            |> Ical.withByHour [ 9 ]
                            |> Ical.withByMinute [ 30 ]
                            |> Ical.withBySecond [ 45 ]
                            |> Ical.withByYearDay [ 100 ]
                            |> Ical.withByWeekNo [ 20 ]
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20240101
DTEND;VALUE=DATE:20240102
DTSTAMP:20240101T000000Z
UID:weekly-all-day-normalized@test.com
SUMMARY:Normalized weekly all-day recurrence
RRULE:FREQ=WEEKLY;BYDAY=MO;BYSETPOS=1
END:VEVENT"""
        , test "generate RRULE drops BYSETPOS when no valid selectors remain" <|
            \() ->
                Ical.event
                    { id = "weekly-all-day-no-selectors"
                    , stamp = toIso8601 "2024-01-01T00:00:00.000Z"
                    , time = Ical.allDay (Date.fromCalendarDate 2024 Time.Jan 1)
                    , summary = "Weekly all-day without selectors"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                            |> Ical.withByDay [ Recurrence.Every2nd Time.Tue ]
                            |> Ical.withByMonthDay [ 1 ]
                            |> Ical.withBySetPos [ 1 ]
                            |> Ical.withByHour [ 9 ]
                            |> Ical.withByMinute [ 30 ]
                            |> Ical.withBySecond [ 45 ]
                            |> Ical.withByYearDay [ 100 ]
                            |> Ical.withByWeekNo [ 20 ]
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART;VALUE=DATE:20240101
DTEND;VALUE=DATE:20240102
DTSTAMP:20240101T000000Z
UID:weekly-all-day-no-selectors@test.com
SUMMARY:Weekly all-day without selectors
RRULE:FREQ=WEEKLY
END:VEVENT"""
        , test "generate RRULE with FREQ=HOURLY" <|
            \() ->
                Ical.event
                    { id = "hourly-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Hourly check"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Hourly { every = 2 })
                            |> Ical.withCount 12
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
UID:hourly-1@test.com
SUMMARY:Hourly check
RRULE:FREQ=HOURLY;INTERVAL=2;COUNT=12
END:VEVENT"""
        , test "generate RRULE with FREQ=MINUTELY" <|
            \() ->
                Ical.event
                    { id = "minutely-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T10:30:00.000Z"
                            }
                    , summary = "Pomodoro"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Minutely { every = 25 }))
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T103000Z
DTSTAMP:20210318T162044Z
UID:minutely-1@test.com
SUMMARY:Pomodoro
RRULE:FREQ=MINUTELY;INTERVAL=25
END:VEVENT"""
        , test "generate RRULE with FREQ=SECONDLY" <|
            \() ->
                Ical.event
                    { id = "secondly-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T10:00:15.000Z"
                            }
                    , summary = "Heartbeat"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Secondly { every = 15 })
                            |> Ical.withCount 100
                        )
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T100000Z
DTEND:20210318T100015Z
DTSTAMP:20210318T162044Z
UID:secondly-1@test.com
SUMMARY:Heartbeat
RRULE:FREQ=SECONDLY;INTERVAL=15;COUNT=100
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
        , test "floating time event" <|
            \() ->
                Ical.event
                    { id = "floating-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.floatingTime
                            { start = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 14, minute = 0, second = 0 }
                            , end = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 15, minute = 30, second = 0 }
                            }
                    , summary = "Local meeting"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T140000
DTEND:20210318T153000
DTSTAMP:20210318T162044Z
UID:floating-1@test.com
SUMMARY:Local meeting
END:VEVENT"""
        , test "floating time with reversed start/end normalizes order" <|
            \() ->
                Ical.event
                    { id = "floating-reversed"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.floatingTime
                            { start = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 15, minute = 0, second = 0 }
                            , end = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 14, minute = 0, second = 0 }
                            }
                    , summary = "Reversed floating"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T140000
DTEND:20210318T150000
DTSTAMP:20210318T162044Z
UID:floating-reversed@test.com
SUMMARY:Reversed floating
END:VEVENT"""
        , test "journal entry with date" <|
            \() ->
                Ical.generateWithJournals
                    (Ical.config
                        { id = "//test//test//EN"
                        , domain = "test.com"
                        }
                    )
                    { events = []
                    , journals =
                        [ Ical.journal
                            { id = "journal-1"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , summary = "Daily standup notes"
                            }
                            |> Ical.withJournalDate (Date.fromCalendarDate 2021 Time.Mar 18)
                            |> Ical.withJournalDescription "Discussed sprint progress."
                        ]
                    }
                    |> expectEqualLines """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//test//test//EN
BEGIN:VJOURNAL
DTSTART;VALUE=DATE:20210318
DTSTAMP:20210318T162044Z
UID:journal-1@test.com
SUMMARY:Daily standup notes
DESCRIPTION:Discussed sprint progress.
END:VJOURNAL
END:VCALENDAR"""
        , test "floating time clamps out-of-range hour/minute/second" <|
            \() ->
                Ical.event
                    { id = "floating-clamped"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.floatingTime
                            { start = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 25, minute = -1, second = 99 }
                            , end = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 25, minute = 70, second = -5 }
                            }
                    , summary = "Clamped floating"
                    }
                    |> Ical.generateEvent
                        (Ical.config
                            { id = "//test//test//EN"
                            , domain = "test.com"
                            }
                        )
                    |> expectEqualLines """BEGIN:VEVENT
DTSTART:20210318T230059
DTEND:20210318T235900
DTSTAMP:20210318T162044Z
UID:floating-clamped@test.com
SUMMARY:Clamped floating
END:VEVENT"""
        , test "event with display alarm 15 minutes before" <|
            \() ->
                Ical.event
                    { id = "alarm-1"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Meeting"
                    }
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "Meeting in 15 minutes"
                            , trigger = Ical.SecondsFromStart (-15 * 60)
                            }
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
UID:alarm-1@test.com
SUMMARY:Meeting
BEGIN:VALARM
TRIGGER:-PT15M
ACTION:DISPLAY
DESCRIPTION:Meeting in 15 minutes
END:VALARM
END:VEVENT"""
        , test "event with audio alarm 1 day before" <|
            \() ->
                Ical.event
                    { id = "alarm-2"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Conference"
                    }
                    |> Ical.withAlarm
                        (Ical.audioAlarm
                            { trigger = Ical.SecondsFromStart (-24 * 60 * 60)
                            }
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
UID:alarm-2@test.com
SUMMARY:Conference
BEGIN:VALARM
TRIGGER:-P1D
ACTION:AUDIO
END:VALARM
END:VEVENT"""
        , test "alarm trigger with mixed duration parts" <|
            \() ->
                Ical.event
                    { id = "alarm-3"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Mixed trigger"
                    }
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "Reminder"
                            , trigger = Ical.SecondsFromStart -(24 * 60 * 60 + 2 * 60 * 60 + 30 * 60)
                            }
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
UID:alarm-3@test.com
SUMMARY:Mixed trigger
BEGIN:VALARM
TRIGGER:-P1DT2H30M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
END:VEVENT"""
        , test "alarm trigger relative to end" <|
            \() ->
                Ical.event
                    { id = "alarm-4"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "End-relative"
                    }
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "Wrapping up"
                            , trigger = Ical.SecondsFromEnd (-5 * 60)
                            }
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
UID:alarm-4@test.com
SUMMARY:End-relative
BEGIN:VALARM
TRIGGER;RELATED=END:-PT5M
ACTION:DISPLAY
DESCRIPTION:Wrapping up
END:VALARM
END:VEVENT"""
        , test "multiple alarms on one event" <|
            \() ->
                Ical.event
                    { id = "alarm-5"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "Multi-alarm"
                    }
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "15 min warning"
                            , trigger = Ical.SecondsFromStart (-15 * 60)
                            }
                        )
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "5 min warning"
                            , trigger = Ical.SecondsFromStart (-5 * 60)
                            }
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
UID:alarm-5@test.com
SUMMARY:Multi-alarm
BEGIN:VALARM
TRIGGER:-PT15M
ACTION:DISPLAY
DESCRIPTION:15 min warning
END:VALARM
BEGIN:VALARM
TRIGGER:-PT5M
ACTION:DISPLAY
DESCRIPTION:5 min warning
END:VALARM
END:VEVENT"""
        , test "alarm with zero duration triggers at event time" <|
            \() ->
                Ical.event
                    { id = "alarm-6"
                    , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                    , time =
                        Ical.withTime
                            { start = toIso8601 "2021-03-18T10:00:00.000Z"
                            , end = toIso8601 "2021-03-18T11:00:00.000Z"
                            }
                    , summary = "At start"
                    }
                    |> Ical.withAlarm
                        (Ical.displayAlarm
                            { description = "Starting now"
                            , trigger = Ical.SecondsFromStart 0
                            }
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
UID:alarm-6@test.com
SUMMARY:At start
BEGIN:VALARM
TRIGGER:PT0S
ACTION:DISPLAY
DESCRIPTION:Starting now
END:VALARM
END:VEVENT"""
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
