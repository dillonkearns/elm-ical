module ExpansionTests exposing (suite)

import Date
import Expect
import Ical.Parser as Parser
import Ical.Recurrence as Recurrence
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Expansion"
        [ describe "no recurrence"
            [ test "event with no RRULE produces single occurrence" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "One-off meeting"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616068800000 -- 2021-03-18T11:00:00Z
                                }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                }
                                [ event ]
                    in
                    Expect.all
                        [ \os -> List.length os |> Expect.equal 1
                        , \os ->
                            os
                                |> List.head
                                |> Maybe.map .time
                                |> Expect.equal (Just event.time)
                        ]
                        occurrences
            , test "event outside date range produces no occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Old event"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2022 Time.Jan 1
                                , end = Date.fromCalendarDate 2022 Time.Dec 31
                                }
                                [ event ]
                    in
                    List.length occurrences |> Expect.equal 0
            ]
        , describe "DAILY"
            [ test "DAILY COUNT=3 produces 3 occurrences on consecutive days" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily standup"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z (Thursday)
                                , end = Time.millisToPosix 1616068800000 -- 2021-03-18T11:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                }
                                [ event ]
                    in
                    Expect.all
                        [ \os -> List.length os |> Expect.equal 3
                        , \os ->
                            os
                                |> List.map (occurrenceDate Time.utc)
                                |> Expect.equal
                                    [ Date.fromCalendarDate 2021 Time.Mar 18
                                    , Date.fromCalendarDate 2021 Time.Mar 19
                                    , Date.fromCalendarDate 2021 Time.Mar 20
                                    ]
                        ]
                        occurrences
            , test "DAILY INTERVAL=2 COUNT=3 produces every-other-day occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Every other day"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 2 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            , Date.fromCalendarDate 2021 Time.Mar 22
                            ]
            , test "DAILY with UNTIL stops at the until date" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily until"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.UntilDate (Date.fromCalendarDate 2021 Time.Mar 20)
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 19
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            ]
            , test "DAILY with UNTIL datetime excludes later same-day occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily until precise time"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616068800000 -- 2021-03-18T11:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.UntilDateTime (Time.millisToPosix 1616144400000) -- 2021-03-19T09:00:00Z
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18 ]
            , test "DAILY with BYDAY filters to specified weekdays" <|
                \() ->
                    let
                        -- 2021-03-18 is a Thursday
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily but only weekdays"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18 Thu
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 5
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Tue, daySpec Time.Wed, daySpec Time.Thu, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18 -- Thu
                            , Date.fromCalendarDate 2021 Time.Mar 19 -- Fri
                            , Date.fromCalendarDate 2021 Time.Mar 22 -- Mon (skips Sat/Sun)
                            , Date.fromCalendarDate 2021 Time.Mar 23 -- Tue
                            , Date.fromCalendarDate 2021 Time.Mar 24 -- Wed
                            ]
            , test "DAILY with BYMONTHDAY=-1 filters to last day of each month" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Last day of month"
                                , start = Time.millisToPosix 1614556800000 -- 2021-03-01T00:00:00Z
                                , end = Time.millisToPosix 1614560400000 -- 2021-03-01T01:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = [ -1 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 1
                                , end = Date.fromCalendarDate 2021 Time.Jun 30
                                }
                                [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 31
                            , Date.fromCalendarDate 2021 Time.Apr 30
                            , Date.fromCalendarDate 2021 Time.May 31
                            , Date.fromCalendarDate 2021 Time.Jun 30
                            ]
            ]
        , describe "WEEKLY"
            [ test "WEEKLY COUNT=3 produces 3 weekly occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Weekly meeting"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18 Thu
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Weekly { every = 1, weekStart = Time.Mon }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 25
                            , Date.fromCalendarDate 2021 Time.Apr 1
                            ]
            , test "WEEKLY with BYDAY=MO,WE,FR COUNT=6" <|
                \() ->
                    let
                        -- 2021-03-15 is a Monday
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "MWF standup"
                                , start = Time.millisToPosix 1615795200000 -- 2021-03-15 Mon
                                , end = Time.millisToPosix 1615798800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Weekly { every = 1, weekStart = Time.Mon }
                                    , end = Recurrence.Count 6
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Wed, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 15 -- Mon
                            , Date.fromCalendarDate 2021 Time.Mar 17 -- Wed
                            , Date.fromCalendarDate 2021 Time.Mar 19 -- Fri
                            , Date.fromCalendarDate 2021 Time.Mar 22 -- Mon
                            , Date.fromCalendarDate 2021 Time.Mar 24 -- Wed
                            , Date.fromCalendarDate 2021 Time.Mar 26 -- Fri
                            ]
            , test "WEEKLY INTERVAL=2 with BYDAY=TU COUNT=3" <|
                \() ->
                    let
                        -- 2021-03-16 is a Tuesday
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Biweekly Tuesday"
                                , start = Time.millisToPosix 1615852800000 -- 2021-03-16 Tue
                                , end = Time.millisToPosix 1615856400000
                                }
                                |> addRule
                                    { frequency = Recurrence.Weekly { every = 2, weekStart = Time.Mon }
                                    , end = Recurrence.Count 3
                                    , byDay = [ daySpec Time.Tue ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 16
                            , Date.fromCalendarDate 2021 Time.Mar 30
                            , Date.fromCalendarDate 2021 Time.Apr 13
                            ]
            ]
        , describe "MONTHLY"
            [ test "MONTHLY COUNT=3 repeats on same day of month" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Monthly on the 18th"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Apr 18
                            , Date.fromCalendarDate 2021 Time.May 18
                            ]
            , test "MONTHLY BYMONTHDAY=1,15 COUNT=4" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "1st and 15th"
                                , start = Time.millisToPosix 1614556800000 -- 2021-03-01
                                , end = Time.millisToPosix 1614560400000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = [ 1, 15 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 1
                            , Date.fromCalendarDate 2021 Time.Mar 15
                            , Date.fromCalendarDate 2021 Time.Apr 1
                            , Date.fromCalendarDate 2021 Time.Apr 15
                            ]
            , test "MONTHLY BYDAY=2MO (second Monday) COUNT=3" <|
                \() ->
                    let
                        -- 2021-03-08 is the second Monday of March
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Second Monday"
                                , start = Time.millisToPosix 1615190400000 -- 2021-03-08
                                , end = Time.millisToPosix 1615194000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = [ Recurrence.second Time.Mon ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 8
                            , Date.fromCalendarDate 2021 Time.Apr 12
                            , Date.fromCalendarDate 2021 Time.May 10
                            ]
            , test "MONTHLY BYDAY=-1FR (last Friday) COUNT=3" <|
                \() ->
                    let
                        -- 2021-03-26 is the last Friday of March
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Last Friday"
                                , start = Time.millisToPosix 1616745600000 -- 2021-03-26
                                , end = Time.millisToPosix 1616749200000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = [ Recurrence.last Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 26
                            , Date.fromCalendarDate 2021 Time.Apr 30
                            , Date.fromCalendarDate 2021 Time.May 28
                            ]
            , test "MONTHLY BYMONTHDAY=-1 (last day of month) COUNT=3" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Last day"
                                , start = Time.millisToPosix 1617148800000 -- 2021-03-31
                                , end = Time.millisToPosix 1617152400000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = [ -1 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 31
                            , Date.fromCalendarDate 2021 Time.Apr 30
                            , Date.fromCalendarDate 2021 Time.May 31
                            ]
            ]
        , describe "YEARLY"
            [ test "YEARLY COUNT=3 repeats on same date" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Anniversary"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2025 Time.Dec 31
                                }
                                [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2022 Time.Mar 18
                            , Date.fromCalendarDate 2023 Time.Mar 18
                            ]
            , test "YEARLY BYMONTH=3,6 BYMONTHDAY=15 COUNT=4" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "March and June 15th"
                                , start = Time.millisToPosix 1615795200000 -- 2021-03-15
                                , end = Time.millisToPosix 1615798800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = [ 15 ]
                                    , byMonth = [ Time.Mar, Time.Jun ]
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2025 Time.Dec 31
                                }
                                [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 15
                            , Date.fromCalendarDate 2021 Time.Jun 15
                            , Date.fromCalendarDate 2022 Time.Mar 15
                            , Date.fromCalendarDate 2022 Time.Jun 15
                            ]
            ]
        , describe "EXDATE"
            [ test "EXDATE excludes specific occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily with exclusion"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 5
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                                |> addExclusion (Time.millisToPosix 1616151600000)
                                -- 2021-03-19
                                |> addExclusion (Time.millisToPosix 1616324400000)

                        -- 2021-03-21
                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            , Date.fromCalendarDate 2021 Time.Mar 22
                            ]
            , test "EXDATE with VALUE=DATE excludes all-day occurrences" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:all-day-exdate@test\u{000D}\nDTSTAMP:20210318T000000Z\u{000D}\nDTSTART;VALUE=DATE:20210318\u{000D}\nRRULE:FREQ=DAILY;COUNT=3\u{000D}\nEXDATE;VALUE=DATE:20210319\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand yearRange [ event ]
                                        |> List.map (occurrenceDate Time.utc)
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 20
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "floating EXDATE excludes floating occurrences" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:floating-exdate@test\u{000D}\nDTSTAMP:20210318T000000Z\u{000D}\nDTSTART:20210318T100000\u{000D}\nRRULE:FREQ=DAILY;COUNT=3\u{000D}\nEXDATE:20210319T100000\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand yearRange [ event ]
                                        |> List.map occurrenceStartDateForFloating
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 20
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "floating EXDATE excludes only the targeted floating occurrence when multiple share a day" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:floating-exdate-same-day@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000"
                                , "DTEND:20240101T093000"
                                , "RRULE:FREQ=DAILY;COUNT=4;BYHOUR=9,10"
                                , "EXDATE:20240102T100000"
                                , "SUMMARY:Floating EXDATE exact occurrence"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand
                                        { start = Date.fromCalendarDate 2024 Time.Jan 1
                                        , end = Date.fromCalendarDate 2024 Time.Jan 2
                                        }
                                        [ event ]
                                        |> List.map occurrenceFloatingDateHour
                                        |> Expect.equal
                                            [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 1, 10 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 2, 9 )
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "EXDATE excludes only the targeted timed occurrence when multiple share a day" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:exdate-one-slot@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000Z"
                                , "DTEND:20240101T100000Z"
                                , "RRULE:FREQ=DAILY;COUNT=4;BYHOUR=9,10"
                                , "EXDATE:20240102T100000Z"
                                , "SUMMARY:Two slots per day"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand
                                        { start = Date.fromCalendarDate 2024 Time.Jan 1
                                        , end = Date.fromCalendarDate 2024 Time.Jan 2
                                        }
                                        [ event ]
                                        |> List.map (\occ -> ( occurrenceDate Time.utc occ, occurrenceHour Time.utc occ ))
                                        |> Expect.equal
                                            [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 1, 10 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 2, 9 )
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            ]
        , describe "RDATE"
            [ test "RDATE adds additional occurrences to expansion" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:rdate-expand@test\u{000D}\nDTSTAMP:20210318T000000Z\u{000D}\nDTSTART:20210318T100000Z\u{000D}\nDTEND:20210318T110000Z\u{000D}\nRRULE:FREQ=DAILY;COUNT=3\u{000D}\nRDATE:20210323T100000Z\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand yearRange [ event ]
                                        |> List.map (occurrenceDate Time.utc)
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 19
                                            , Date.fromCalendarDate 2021 Time.Mar 20
                                            , Date.fromCalendarDate 2021 Time.Mar 23
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "RDATE adds occurrences to non-recurring event" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:rdate-no-rrule@test\u{000D}\nDTSTAMP:20210318T000000Z\u{000D}\nDTSTART:20210318T100000Z\u{000D}\nDTEND:20210318T110000Z\u{000D}\nRDATE:20210319T100000Z,20210320T100000Z\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand yearRange [ event ]
                                        |> List.map (occurrenceDate Time.utc)
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 19
                                            , Date.fromCalendarDate 2021 Time.Mar 20
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "timed RDATE preserves its explicit time-of-day during expansion" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:rdate-time@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T100000Z"
                                , "DTEND:20240101T110000Z"
                                , "RDATE:20240102T140000Z"
                                , "SUMMARY:RDATE explicit time"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand
                                        { start = Date.fromCalendarDate 2024 Time.Jan 1
                                        , end = Date.fromCalendarDate 2024 Time.Jan 3
                                        }
                                        [ event ]
                                        |> List.map (\occ -> ( occurrenceDate Time.utc occ, occurrenceHour Time.utc occ ))
                                        |> Expect.equal
                                            [ ( Date.fromCalendarDate 2024 Time.Jan 1, 10 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 2, 14 )
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "floating RDATE preserves its explicit local time-of-day during expansion" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:floating-rdate@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000"
                                , "DTEND:20240101T100000"
                                , "RDATE:20240102T140000"
                                , "SUMMARY:Floating RDATE explicit time"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand
                                        { start = Date.fromCalendarDate 2024 Time.Jan 1
                                        , end = Date.fromCalendarDate 2024 Time.Jan 2
                                        }
                                        [ event ]
                                        |> List.map occurrenceFloatingDateHour
                                        |> Expect.equal
                                            [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 2, 14 )
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "expandNext includes RDATE occurrences" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:rdate-next@test\u{000D}\nDTSTAMP:20210318T000000Z\u{000D}\nDTSTART:20210318T100000Z\u{000D}\nDTEND:20210318T110000Z\u{000D}\nRRULE:FREQ=DAILY;COUNT=2\u{000D}\nRDATE:20210323T100000Z\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expandNext 5
                                        (Date.fromCalendarDate 2021 Time.Mar 18)
                                        [ event ]
                                        |> List.map (occurrenceDate Time.utc)
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 19
                                            , Date.fromCalendarDate 2021 Time.Mar 23
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            ]
        , describe "BYSETPOS"
            [ test "MONTHLY BYDAY=MO,TU,WE,TH,FR BYSETPOS=-1 (last weekday) COUNT=3" <|
                \() ->
                    let
                        -- 2021-03-31 is the last weekday (Wednesday) of March
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Last weekday"
                                , start = Time.millisToPosix 1617148800000 -- 2021-03-31
                                , end = Time.millisToPosix 1617152400000
                                }
                                |> addRule
                                    { frequency = Recurrence.Monthly { every = 1 }
                                    , end = Recurrence.Count 3
                                    , byDay =
                                        [ daySpec Time.Mon
                                        , daySpec Time.Tue
                                        , daySpec Time.Wed
                                        , daySpec Time.Thu
                                        , daySpec Time.Fri
                                        ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = [ -1 ]
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 31 -- Wed
                            , Date.fromCalendarDate 2021 Time.Apr 30 -- Fri
                            , Date.fromCalendarDate 2021 Time.May 31 -- Mon
                            ]
            ]
        , describe "AllDay events"
            [ test "AllDay WEEKLY COUNT=3" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeAllDayEvent
                                { summary = "Weekly all-day"
                                , start = Date.fromCalendarDate 2021 Time.Mar 18
                                , end = Date.fromCalendarDate 2021 Time.Mar 18
                                }
                                |> addRule
                                    { frequency = Recurrence.Weekly { every = 1, weekStart = Time.Mon }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    Expect.all
                        [ \os -> List.length os |> Expect.equal 3
                        , \os ->
                            os
                                |> List.map (occurrenceDate Time.utc)
                                |> Expect.equal
                                    [ Date.fromCalendarDate 2021 Time.Mar 18
                                    , Date.fromCalendarDate 2021 Time.Mar 25
                                    , Date.fromCalendarDate 2021 Time.Apr 1
                                    ]
                        , \os ->
                            -- Each occurrence should be AllDay
                            os
                                |> List.all
                                    (\o ->
                                        case o.time of
                                            Parser.AllDay _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                |> Expect.equal True
                        ]
                        occurrences
            ]
        , describe "time shifting"
            [ test "WithTime occurrences preserve time-of-day and duration" <|
                \() ->
                    let
                        -- 10:00-11:30 UTC
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Timed event"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616070600000 -- 2021-03-18T11:30:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 2
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange [ event ]
                    in
                    case occurrences of
                        [ _, second ] ->
                            case second.time of
                                Parser.WithTime { start, end } ->
                                    Expect.all
                                        [ \_ ->
                                            -- Next day same time: 2021-03-19T10:00:00Z
                                            Time.posixToMillis start.posix
                                                |> Expect.equal (1616065200000 + 86400000)
                                        , \_ ->
                                            -- Next day same end: 2021-03-19T11:30:00Z
                                            end
                                                |> Maybe.map (\e -> Time.posixToMillis e.posix)
                                                |> Expect.equal (Just (1616070600000 + 86400000))
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "Expected WithTime"

                        _ ->
                            Expect.fail ("Expected 2 occurrences, got " ++ String.fromInt (List.length occurrences))
            ]
        , describe "Forever"
            [ test "Forever is bounded by the date range" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Forever daily"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 18
                                , end = Date.fromCalendarDate 2021 Time.Mar 22
                                }
                                [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 19
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            , Date.fromCalendarDate 2021 Time.Mar 21
                            , Date.fromCalendarDate 2021 Time.Mar 22
                            ]
            ]
        , describe "COUNT with late date range"
            [ test "COUNT occurrences outside range are counted but not emitted" <|
                \() ->
                    let
                        -- Event starts 2021-03-18, COUNT=10 daily → last is 2021-03-27
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Count with late range"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 10
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 25
                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                }
                                [ event ]
                    in
                    occurrences
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 25
                            , Date.fromCalendarDate 2021 Time.Mar 26
                            , Date.fromCalendarDate 2021 Time.Mar 27
                            ]
            ]
        , describe "parse then expand round-trip"
            [ test "parse ICS with RRULE then expand" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:rrule-expand@test\u{000D}\nDTSTAMP:20210318T162044Z\u{000D}\nDTSTART:20210318T100000Z\u{000D}\nDTEND:20210318T110000Z\u{000D}\nSUMMARY:Weekly standup\u{000D}\nRRULE:FREQ=WEEKLY;COUNT=4\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ ev ] ->
                                    let
                                        occurrences : List Parser.Occurrence
                                        occurrences =
                                            Parser.expand
                                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                                }
                                                [ ev ]
                                    in
                                    Expect.all
                                        [ \os -> List.length os |> Expect.equal 4
                                        , \os ->
                                            os
                                                |> List.map (occurrenceDate Time.utc)
                                                |> Expect.equal
                                                    [ Date.fromCalendarDate 2021 Time.Mar 18
                                                    , Date.fromCalendarDate 2021 Time.Mar 25
                                                    , Date.fromCalendarDate 2021 Time.Apr 1
                                                    , Date.fromCalendarDate 2021 Time.Apr 8
                                                    ]
                                        , \os ->
                                            os
                                                |> List.map (\o -> o.event.summary)
                                                |> List.head
                                                |> Maybe.andThen identity
                                                |> Expect.equal (Just "Weekly standup")
                                        ]
                                        occurrences

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            , test "TZID weekly recurrence near midnight follows the local weekday" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VTIMEZONE"
                                , "TZID:America/New_York"
                                , "BEGIN:DAYLIGHT"
                                , "TZOFFSETFROM:-0500"
                                , "TZOFFSETTO:-0400"
                                , "DTSTART:19700308T020000"
                                , "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU"
                                , "END:DAYLIGHT"
                                , "BEGIN:STANDARD"
                                , "TZOFFSETFROM:-0400"
                                , "TZOFFSETTO:-0500"
                                , "DTSTART:19701101T020000"
                                , "RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU"
                                , "END:STANDARD"
                                , "END:VTIMEZONE"
                                , "BEGIN:VEVENT"
                                , "UID:late-night-monday@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART;TZID=America/New_York:20240101T233000"
                                , "DTEND;TZID=America/New_York:20240102T003000"
                                , "RRULE:FREQ=WEEKLY;COUNT=2;BYDAY=MO"
                                , "SUMMARY:Late Monday event"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 31
                                }
                                cal.events
                                |> List.map (\occ -> ( occurrenceDate Time.utc occ, occurrenceHourMinute Time.utc occ ))
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2024 Time.Jan 2, ( 4, 30 ) )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 9, ( 4, 30 ) )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "parse ICS with RRULE and EXDATE then expand" <|
                \() ->
                    let
                        input : String
                        input =
                            "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//Test//EN\u{000D}\nBEGIN:VEVENT\u{000D}\nUID:rrule-exdate@test\u{000D}\nDTSTAMP:20210318T162044Z\u{000D}\nDTSTART:20210318T100000Z\u{000D}\nDTEND:20210318T110000Z\u{000D}\nSUMMARY:Daily with skip\u{000D}\nRRULE:FREQ=DAILY;COUNT=5\u{000D}\nEXDATE:20210319T100000Z\u{000D}\nEXDATE:20210321T100000Z\u{000D}\nEND:VEVENT\u{000D}\nEND:VCALENDAR\u{000D}\n"
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ ev ] ->
                                    let
                                        occurrences : List Parser.Occurrence
                                        occurrences =
                                            Parser.expand yearRange [ ev ]
                                    in
                                    occurrences
                                        |> List.map (occurrenceDate Time.utc)
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 20
                                            , Date.fromCalendarDate 2021 Time.Mar 22
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            ]
        , describe "expandNext"
            [ test "next 3 daily occurrences from DTSTART" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 19
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            ]
            , test "next 3 from a later date skips earlier occurrences" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 25)
                        [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 25
                            , Date.fromCalendarDate 2021 Time.Mar 26
                            , Date.fromCalendarDate 2021 Time.Mar 27
                            ]
            , test "respects COUNT even when asking for more" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Limited"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 2
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 10
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        [ event ]
                        |> List.length
                        |> Expect.equal 2
            , test "non-recurring event returns single occurrence if on or after start" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "One-off"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                    in
                    Expect.all
                        [ \_ ->
                            Parser.expandNext 5
                                (Date.fromCalendarDate 2021 Time.Mar 18)
                                [ event ]
                                |> List.length
                                |> Expect.equal 1
                        , \_ ->
                            Parser.expandNext 5
                                (Date.fromCalendarDate 2021 Time.Mar 1)
                                [ event ]
                                |> List.length
                                |> Expect.equal 1
                        , \_ ->
                            Parser.expandNext 5
                                (Date.fromCalendarDate 2021 Time.Mar 20)
                                [ event ]
                                |> List.length
                                |> Expect.equal 0
                        ]
                        ()
            , test "weekly with BYDAY from a later date" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "MWF"
                                , start = Time.millisToPosix 1615795200000 -- 2021-03-15 Mon
                                , end = Time.millisToPosix 1615798800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Weekly { every = 1, weekStart = Time.Mon }
                                    , end = Recurrence.Forever
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Wed, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 4
                        (Date.fromCalendarDate 2021 Time.Mar 24)
                        [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 24 -- Wed
                            , Date.fromCalendarDate 2021 Time.Mar 26 -- Fri
                            , Date.fromCalendarDate 2021 Time.Mar 29 -- Mon
                            , Date.fromCalendarDate 2021 Time.Mar 31 -- Wed
                            ]
            , test "with EXDATE exclusions" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Daily with exclusions"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                                |> addExclusion (Time.millisToPosix 1616151600000)
                                -- 2021-03-19
                                |> addExclusion (Time.millisToPosix 1616324400000)

                        -- 2021-03-21
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            , Date.fromCalendarDate 2021 Time.Mar 22
                            ]
            , test "expandNext with HOURLY frequency" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Hourly check"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Hourly { every = 3 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 4
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        [ event ]
                        |> List.map (occurrenceHour Time.utc)
                        |> Expect.equal [ 11, 14, 17, 20 ]
            , test "expandNext with DAILY + BYHOUR" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Twice daily"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = [ 9, 17 ]
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expandNext 4
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        [ event ]
                        |> List.map (occurrenceHour Time.utc)
                        |> Expect.equal [ 9, 17, 9, 17 ]
            ]
        , describe "sub-daily with BY* filters"
            [ test "HOURLY with BYDAY filter limits to specific days" <|
                \() ->
                    let
                        -- 2021-03-18 is a Thursday
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Weekday hourly"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z (Thu)
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Hourly { every = 12 }
                                    , end = Recurrence.Count 4
                                    , byDay = [ daySpec Time.Thu, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (\occ -> ( occurrenceDate Time.utc occ, occurrenceHour Time.utc occ ))
                        |> Expect.equal
                            [ ( Date.fromCalendarDate 2021 Time.Mar 18, 11 ) -- Thu 11:00
                            , ( Date.fromCalendarDate 2021 Time.Mar 18, 23 ) -- Thu 23:00
                            , ( Date.fromCalendarDate 2021 Time.Mar 19, 11 ) -- Fri 11:00
                            , ( Date.fromCalendarDate 2021 Time.Mar 19, 23 ) -- Fri 23:00
                            ]
            , test "MINUTELY with BYHOUR filter" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Office hours pings"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z
                                , end = Time.millisToPosix 1616066100000
                                }
                                |> addRule
                                    { frequency = Recurrence.Minutely { every = 30 }
                                    , end = Recurrence.Count 6
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = [ 11, 12 ]
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceHourMinute Time.utc)
                        |> Expect.equal
                            [ ( 11, 0 ), ( 11, 30 ), ( 12, 0 ), ( 12, 30 ), ( 11, 0 ), ( 11, 30 ) ]
            ]
        , describe "FloatingTime with BYHOUR"
            [ test "DAILY with BYHOUR on floating time event" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeFloatingEvent
                                { summary = "Local twice daily"
                                , year = 2021
                                , month = Time.Mar
                                , day = 18
                                , hour = 10
                                , minute = 0
                                , second = 0
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = [ 9, 17 ]
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map occurrenceFloatingHour
                        |> Expect.equal [ 9, 17, 9, 17 ]
            ]
        , describe "Floating sub-daily recurrence"
            [ test "HOURLY recurrence expands in local floating time" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:floating-hourly@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000"
                                , "DTEND:20240101T100000"
                                , "RRULE:FREQ=HOURLY;COUNT=3"
                                , "SUMMARY:Floating hourly"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            case cal.events of
                                [ event ] ->
                                    Parser.expand
                                        { start = Date.fromCalendarDate 2024 Time.Jan 1
                                        , end = Date.fromCalendarDate 2024 Time.Jan 1
                                        }
                                        [ event ]
                                        |> List.map occurrenceFloatingDateHour
                                        |> Expect.equal
                                            [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 1, 10 )
                                            , ( Date.fromCalendarDate 2024 Time.Jan 1, 11 )
                                            ]

                                _ ->
                                    Expect.fail "Expected 1 event"

                        Err err ->
                            Expect.fail err
            ]
        , describe "BYYEARDAY"
            [ test "YEARLY with BYYEARDAY expands to specific days of the year" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Yearly milestones"
                                , start = Time.millisToPosix 1609502400000 -- 2021-01-01T10:00:00Z
                                , end = Time.millisToPosix 1609506000000 -- 2021-01-01T11:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = [ 1, 100 ]
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Jan 1
                            , Date.fromCalendarDate 2021 Time.Apr 10
                            ]
            , test "YEARLY with negative BYYEARDAY (-1 = last day of year)" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Year end"
                                , start = Time.millisToPosix 1609502400000 -- 2021-01-01T10:00:00Z
                                , end = Time.millisToPosix 1609506000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = [ -1 ]
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Dec 31
                            ]
            ]
        , describe "YEARLY BYDAY"
            [ test "YEARLY with BYDAY and no BYMONTH expands across the whole year" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Every Monday this year"
                                , start = Time.millisToPosix 1609754400000 -- 2021-01-04T10:00:00Z
                                , end = Time.millisToPosix 1609758000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = [ daySpec Time.Mon ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = [ 1, 20, -1 ]
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Jan 4
                            , Date.fromCalendarDate 2021 Time.May 17
                            , Date.fromCalendarDate 2021 Time.Dec 27
                            ]
            , test "YEARLY with numeric BYDAY and no BYMONTH uses the year ordinal" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "20th Monday of the year"
                                , start = Time.millisToPosix 1609754400000 -- 2021-01-04T10:00:00Z
                                , end = Time.millisToPosix 1609758000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = [ nthDaySpec 20 Time.Mon ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.May 17
                            ]
            , test "YEARLY with BYMONTHDAY and no BYMONTH expands across all months" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "First day of each month"
                                , start = Time.millisToPosix 1609502400000 -- 2021-01-01T10:00:00Z
                                , end = Time.millisToPosix 1609506000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = [ 1 ]
                                    , byMonth = []
                                    , bySetPos = [ 1, 6, -1 ]
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Jan 1
                            , Date.fromCalendarDate 2021 Time.Jun 1
                            , Date.fromCalendarDate 2021 Time.Dec 1
                            ]
            ]
        , describe "BYWEEKNO"
            [ test "YEARLY with BYWEEKNO and BYDAY expands to specific ISO weeks" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "ISO week event"
                                , start = Time.millisToPosix 1609754400000 -- 2021-01-04T10:00:00Z (Mon, ISO week 1)
                                , end = Time.millisToPosix 1609758000000
                                }
                                |> addRule
                                    { frequency = Recurrence.Yearly { every = 1 }
                                    , end = Recurrence.Forever
                                    , byDay = [ daySpec Time.Mon ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = [ 1, 10 ]
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Jan 4 -- ISO week 1 Monday
                            , Date.fromCalendarDate 2021 Time.Mar 8 -- ISO week 10 Monday
                            ]
            ]
        , describe "BYHOUR/BYMINUTE/BYSECOND"
            [ test "DAILY with BYHOUR produces multiple occurrences per day" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Twice daily"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616068800000 -- 2021-03-18T11:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = [ 9, 17 ]
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceHour Time.utc)
                        |> Expect.equal [ 9, 17, 9, 17 ]
            , test "DAILY with BYHOUR and BYMINUTE produces cartesian product" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Multi-time"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616068800000
                                }
                                |> addRule
                                    { frequency = Recurrence.Daily { every = 1 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = [ 9, 17 ]
                                    , byMinute = [ 0, 30 ]
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceHourMinute Time.utc)
                        |> Expect.equal [ ( 9, 0 ), ( 9, 30 ), ( 17, 0 ), ( 17, 30 ) ]
            ]
        , describe "sub-daily frequencies"
            [ test "HOURLY expansion produces occurrences across days" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Hourly check"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z
                                , end = Time.millisToPosix 1616068800000 -- 2021-03-18T12:00:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Hourly { every = 6 }
                                    , end = Recurrence.Count 5
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceHour Time.utc)
                        |> Expect.equal [ 11, 17, 23, 5, 11 ]
            , test "MINUTELY expansion with COUNT" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Pomodoro"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T11:00:00Z
                                , end = Time.millisToPosix 1616066700000 -- 2021-03-18T11:25:00Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Minutely { every = 30 }
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceHourMinute Time.utc)
                        |> Expect.equal [ ( 11, 0 ), ( 11, 30 ), ( 12, 0 ), ( 12, 30 ) ]
            , test "SECONDLY expansion with COUNT" <|
                \() ->
                    let
                        event : Parser.Event
                        event =
                            makeTimedEvent
                                { summary = "Heartbeat"
                                , start = Time.millisToPosix 1616065200000 -- 2021-03-18T10:00:00Z
                                , end = Time.millisToPosix 1616065215000 -- 2021-03-18T10:00:15Z
                                }
                                |> addRule
                                    { frequency = Recurrence.Secondly { every = 15 }
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , byHour = []
                                    , byMinute = []
                                    , bySecond = []
                                    , byYearDay = []
                                    , byWeekNo = []
                                    }
                    in
                    Parser.expand yearRange [ event ]
                        |> List.map (occurrenceSecond Time.utc)
                        |> Expect.equal [ 0, 15, 30 ]
            ]
        , describe "RECURRENCE-ID overrides"
            [ test "override replaces original occurrence in expand" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:override-test@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210318T100000Z"
                                , "DTEND:20210318T110000Z"
                                , "SUMMARY:Weekly standup"
                                , "RRULE:FREQ=WEEKLY;COUNT=4"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:override-test@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210325T140000Z"
                                , "DTEND:20210325T150000Z"
                                , "SUMMARY:Weekly standup (moved)"
                                , "RECURRENCE-ID:20210325T100000Z"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            let
                                occurrences : List Parser.Occurrence
                                occurrences =
                                    Parser.expand yearRange cal.events
                            in
                            occurrences
                                |> List.map (\occ -> ( occurrenceDate Time.utc occ, occ.event.summary ))
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2021 Time.Mar 18, Just "Weekly standup" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 25, Just "Weekly standup (moved)" )
                                    , ( Date.fromCalendarDate 2021 Time.Apr 1, Just "Weekly standup" )
                                    , ( Date.fromCalendarDate 2021 Time.Apr 8, Just "Weekly standup" )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "cancelled override removes occurrence from expansion" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:cancel-test@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210318T100000Z"
                                , "DTEND:20210318T110000Z"
                                , "SUMMARY:Weekly standup"
                                , "RRULE:FREQ=WEEKLY;COUNT=3"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:cancel-test@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210325T100000Z"
                                , "DTEND:20210325T110000Z"
                                , "SUMMARY:Weekly standup"
                                , "STATUS:CANCELLED"
                                , "RECURRENCE-ID:20210325T100000Z"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            let
                                occurrences : List Parser.Occurrence
                                occurrences =
                                    Parser.expand yearRange cal.events
                                        |> List.filter (\occ -> occ.event.status /= Just Parser.Cancelled)
                            in
                            occurrences
                                |> List.map (occurrenceDate Time.utc)
                                |> Expect.equal
                                    [ Date.fromCalendarDate 2021 Time.Mar 18
                                    , Date.fromCalendarDate 2021 Time.Apr 1
                                    ]

                        Err err ->
                            Expect.fail err
            , test "multiple overrides on same master event" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:multi-override@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210318T100000Z"
                                , "DTEND:20210318T110000Z"
                                , "SUMMARY:Daily standup"
                                , "RRULE:FREQ=DAILY;COUNT=5"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:multi-override@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210319T140000Z"
                                , "DTEND:20210319T150000Z"
                                , "SUMMARY:Daily standup (moved to afternoon)"
                                , "RECURRENCE-ID:20210319T100000Z"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:multi-override@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210321T090000Z"
                                , "DTEND:20210321T100000Z"
                                , "SUMMARY:Daily standup (moved earlier)"
                                , "RECURRENCE-ID:20210321T100000Z"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand yearRange cal.events
                                |> List.map (\occ -> ( occurrenceDate Time.utc occ, occ.event.summary ))
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2021 Time.Mar 18, Just "Daily standup" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 19, Just "Daily standup (moved to afternoon)" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 20, Just "Daily standup" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 21, Just "Daily standup (moved earlier)" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 22, Just "Daily standup" )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "override does not affect unrelated events" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:event-a@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210318T100000Z"
                                , "DTEND:20210318T110000Z"
                                , "SUMMARY:Event A"
                                , "RRULE:FREQ=WEEKLY;COUNT=2"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:event-b@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210318T100000Z"
                                , "DTEND:20210318T110000Z"
                                , "SUMMARY:Event B"
                                , "RRULE:FREQ=WEEKLY;COUNT=2"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:event-a@test"
                                , "DTSTAMP:20210318T000000Z"
                                , "DTSTART:20210325T140000Z"
                                , "DTEND:20210325T150000Z"
                                , "SUMMARY:Event A (moved)"
                                , "RECURRENCE-ID:20210325T100000Z"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand yearRange cal.events
                                |> List.map (\occ -> ( occurrenceDate Time.utc occ, occ.event.summary ))
                                |> List.sortBy (\( _, s ) -> Maybe.withDefault "" s)
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2021 Time.Mar 18, Just "Event A" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 25, Just "Event A (moved)" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 18, Just "Event B" )
                                    , ( Date.fromCalendarDate 2021 Time.Mar 25, Just "Event B" )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "override replaces only the targeted timed occurrence when multiple share a day" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:override-one-slot@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000Z"
                                , "DTEND:20240101T100000Z"
                                , "RRULE:FREQ=DAILY;COUNT=4;BYHOUR=9,10"
                                , "SUMMARY:Original slot"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:override-one-slot@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240102T150000Z"
                                , "DTEND:20240102T160000Z"
                                , "SUMMARY:Moved 10am slot"
                                , "RECURRENCE-ID:20240102T100000Z"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 2
                                }
                                cal.events
                                |> List.map (\occ -> ( occurrenceDate Time.utc occ, occurrenceHour Time.utc occ, occ.event.summary ))
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9, Just "Original slot" )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 1, 10, Just "Original slot" )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 2, 9, Just "Original slot" )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 2, 15, Just "Moved 10am slot" )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "floating override replaces only the targeted floating occurrence when multiple share a day" <|
                \() ->
                    let
                        input : String
                        input =
                            String.join "\u{000D}\n"
                                [ "BEGIN:VCALENDAR"
                                , "VERSION:2.0"
                                , "PRODID:-//Test//EN"
                                , "BEGIN:VEVENT"
                                , "UID:floating-override@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000"
                                , "DTEND:20240101T093000"
                                , "RRULE:FREQ=DAILY;COUNT=4;BYHOUR=9,10"
                                , "SUMMARY:Floating master"
                                , "END:VEVENT"
                                , "BEGIN:VEVENT"
                                , "UID:floating-override@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "RECURRENCE-ID:20240102T100000"
                                , "DTSTART:20240102T140000"
                                , "DTEND:20240102T143000"
                                , "SUMMARY:Floating override"
                                , "END:VEVENT"
                                , "END:VCALENDAR"
                                , ""
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 2
                                }
                                cal.events
                                |> List.map occurrenceFloatingDateHour
                                |> Expect.equal
                                    [ ( Date.fromCalendarDate 2024 Time.Jan 1, 9 )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 1, 10 )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 2, 9 )
                                    , ( Date.fromCalendarDate 2024 Time.Jan 2, 14 )
                                    ]

                        Err err ->
                            Expect.fail err
            ]
        ]



-- HELPERS


yearRange : { start : Date.Date, end : Date.Date }
yearRange =
    { start = Date.fromCalendarDate 2021 Time.Jan 1
    , end = Date.fromCalendarDate 2021 Time.Dec 31
    }


makeAllDayEvent : { summary : String, start : Date.Date, end : Date.Date } -> Parser.Event
makeAllDayEvent { summary, start, end } =
    { uid = "test-uid"
    , stamp = Time.millisToPosix 0
    , time = Parser.AllDay { start = start, end = Just end }
    , created = Nothing
    , lastModified = Nothing
    , summary = Just summary
    , description = Nothing
    , location = Nothing
    , organizer = Nothing
    , status = Nothing
    , transparency = Nothing
    , recurrenceRules = []
    , exclusions = []
    , recurrenceDates = []
    , recurrenceId = Nothing
    , attendees = []
    , alarms = []
    , extraProperties = []
    }


daySpec : Time.Weekday -> Recurrence.DaySpec
daySpec weekday =
    Recurrence.every weekday


nthDaySpec : Int -> Time.Weekday -> Recurrence.DaySpec
nthDaySpec n weekday =
    case Recurrence.nth n weekday of
        Just spec ->
            spec

        Nothing ->
            Debug.todo ("Invalid day spec ordinal in test: " ++ String.fromInt n)


addExclusion : Time.Posix -> Parser.Event -> Parser.Event
addExclusion posix event =
    { event | exclusions = Parser.AtTime (utcResolved posix) :: event.exclusions }


makeTimedEvent : { summary : String, start : Time.Posix, end : Time.Posix } -> Parser.Event
makeTimedEvent { summary, start, end } =
    { uid = "test-uid"
    , stamp = start
    , time =
        Parser.WithTime
            { start = utcResolved start
            , end = Just (utcResolved end)
            }
    , created = Nothing
    , lastModified = Nothing
    , summary = Just summary
    , description = Nothing
    , location = Nothing
    , organizer = Nothing
    , status = Nothing
    , transparency = Nothing
    , recurrenceRules = []
    , exclusions = []
    , recurrenceDates = []
    , recurrenceId = Nothing
    , attendees = []
    , alarms = []
    , extraProperties = []
    }


occurrenceStartDateForFloating : Parser.Occurrence -> Date.Date
occurrenceStartDateForFloating occurrence =
    case occurrence.time of
        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year start.month start.day

        _ ->
            occurrenceDate Time.utc occurrence


addRule : Recurrence.RecurrenceRule -> Parser.Event -> Parser.Event
addRule rule event =
    { event | recurrenceRules = rule :: event.recurrenceRules }


makeFloatingEvent : { summary : String, year : Int, month : Time.Month, day : Int, hour : Int, minute : Int, second : Int } -> Parser.Event
makeFloatingEvent { summary, year, month, day, hour, minute, second } =
    { uid = "test-uid"
    , stamp = Time.millisToPosix 0
    , time =
        Parser.FloatingTime
            { start = { year = year, month = month, day = day, hour = hour, minute = minute, second = second }
            , end = Just { year = year, month = month, day = day, hour = hour + 1, minute = minute, second = second }
            }
    , created = Nothing
    , lastModified = Nothing
    , summary = Just summary
    , description = Nothing
    , location = Nothing
    , organizer = Nothing
    , status = Nothing
    , transparency = Nothing
    , recurrenceRules = []
    , exclusions = []
    , recurrenceDates = []
    , recurrenceId = Nothing
    , attendees = []
    , alarms = []
    , extraProperties = []
    }


occurrenceFloatingHour : Parser.Occurrence -> Int
occurrenceFloatingHour occ =
    case occ.time of
        Parser.FloatingTime { start } ->
            start.hour

        _ ->
            -1


occurrenceFloatingDateHour : Parser.Occurrence -> ( Date.Date, Int )
occurrenceFloatingDateHour occ =
    case occ.time of
        Parser.FloatingTime { start } ->
            ( Date.fromCalendarDate start.year start.month start.day, start.hour )

        _ ->
            ( Date.fromCalendarDate 1900 Time.Jan 1, -1 )


occurrenceDate : Time.Zone -> Parser.Occurrence -> Date.Date
occurrenceDate zone occ =
    case occ.time of
        Parser.WithTime { start } ->
            Date.fromPosix zone start.posix

        Parser.AllDay { start } ->
            start

        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year start.month start.day


occurrenceHour : Time.Zone -> Parser.Occurrence -> Int
occurrenceHour zone occ =
    case occ.time of
        Parser.WithTime { start } ->
            Time.toHour zone start.posix

        _ ->
            -1


occurrenceHourMinute : Time.Zone -> Parser.Occurrence -> ( Int, Int )
occurrenceHourMinute zone occ =
    case occ.time of
        Parser.WithTime { start } ->
            ( Time.toHour zone start.posix, Time.toMinute zone start.posix )

        _ ->
            ( -1, -1 )


occurrenceSecond : Time.Zone -> Parser.Occurrence -> Int
occurrenceSecond zone occ =
    case occ.time of
        Parser.WithTime { start } ->
            Time.toSecond zone start.posix

        _ ->
            -1


utcResolved : Time.Posix -> Parser.ResolvedTime
utcResolved posix =
    { posix = posix
    , timeZoneName = Nothing
    , localDateTime = Nothing
    , timeZoneContext = Nothing
    }
