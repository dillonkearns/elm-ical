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
                                event
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
                                event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                }
                                event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 2
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.UntilDate (Date.fromCalendarDate 2021 Time.Mar 20)
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.UntilDateTime (Time.millisToPosix 1616144400000) -- 2021-03-19T09:00:00Z
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                    in
                    Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 5
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Tue, daySpec Time.Wed, daySpec Time.Thu, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = [ -1 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 1
                                , end = Date.fromCalendarDate 2021 Time.Jun 30
                                }
                                event
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
                                    { frequency = Recurrence.Weekly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Weekly
                                    , interval = 1
                                    , end = Recurrence.Count 6
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Wed, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Weekly
                                    , interval = 2
                                    , end = Recurrence.Count 3
                                    , byDay = [ daySpec Time.Tue ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = [ 1, 15 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = [ { ordinal = Just 2, weekday = Time.Mon } ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = [ { ordinal = Just -1, weekday = Time.Fri } ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = [ -1 ]
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Yearly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2025 Time.Dec 31
                                }
                                event
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
                                    { frequency = Recurrence.Yearly
                                    , interval = 1
                                    , end = Recurrence.Count 4
                                    , byDay = []
                                    , byMonthDay = [ 15 ]
                                    , byMonth = [ Time.Mar, Time.Jun ]
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Jan 1
                                , end = Date.fromCalendarDate 2025 Time.Dec 31
                                }
                                event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 5
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                                |> addExclusion (Time.millisToPosix 1616151600000)
                                -- 2021-03-19
                                |> addExclusion (Time.millisToPosix 1616324400000)

                        -- 2021-03-21
                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    Parser.expand yearRange event
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
                                    Parser.expand yearRange event
                                        |> List.map occurrenceStartDateForFloating
                                        |> Expect.equal
                                            [ Date.fromCalendarDate 2021 Time.Mar 18
                                            , Date.fromCalendarDate 2021 Time.Mar 20
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
                                    { frequency = Recurrence.Monthly
                                    , interval = 1
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
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Weekly
                                    , interval = 1
                                    , end = Recurrence.Count 3
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 2
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand yearRange event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 18
                                , end = Date.fromCalendarDate 2021 Time.Mar 22
                                }
                                event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 10
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }

                        occurrences : List Parser.Occurrence
                        occurrences =
                            Parser.expand
                                { start = Date.fromCalendarDate 2021 Time.Mar 25
                                , end = Date.fromCalendarDate 2021 Time.Dec 31
                                }
                                event
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
                                                ev
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
                                            Parser.expand yearRange ev
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 25)
                        event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Count 2
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                    in
                    Parser.expandNext 10
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        event
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
                                event
                                |> List.length
                                |> Expect.equal 1
                        , \_ ->
                            Parser.expandNext 5
                                (Date.fromCalendarDate 2021 Time.Mar 1)
                                event
                                |> List.length
                                |> Expect.equal 1
                        , \_ ->
                            Parser.expandNext 5
                                (Date.fromCalendarDate 2021 Time.Mar 20)
                                event
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
                                    { frequency = Recurrence.Weekly
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = [ daySpec Time.Mon, daySpec Time.Wed, daySpec Time.Fri ]
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                    in
                    Parser.expandNext 4
                        (Date.fromCalendarDate 2021 Time.Mar 24)
                        event
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
                                    { frequency = Recurrence.Daily
                                    , interval = 1
                                    , end = Recurrence.Forever
                                    , byDay = []
                                    , byMonthDay = []
                                    , byMonth = []
                                    , bySetPos = []
                                    , weekStart = Time.Mon
                                    }
                                |> addExclusion (Time.millisToPosix 1616151600000)
                                -- 2021-03-19
                                |> addExclusion (Time.millisToPosix 1616324400000)

                        -- 2021-03-21
                    in
                    Parser.expandNext 3
                        (Date.fromCalendarDate 2021 Time.Mar 18)
                        event
                        |> List.map (occurrenceDate Time.utc)
                        |> Expect.equal
                            [ Date.fromCalendarDate 2021 Time.Mar 18
                            , Date.fromCalendarDate 2021 Time.Mar 20
                            , Date.fromCalendarDate 2021 Time.Mar 22
                            ]
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
    , attendees = []
    , extraProperties = []
    }


daySpec : Time.Weekday -> Recurrence.DaySpec
daySpec weekday =
    { ordinal = Nothing, weekday = weekday }


addExclusion : Time.Posix -> Parser.Event -> Parser.Event
addExclusion posix event =
    { event | exclusions = posix :: event.exclusions }


makeTimedEvent : { summary : String, start : Time.Posix, end : Time.Posix } -> Parser.Event
makeTimedEvent { summary, start, end } =
    { uid = "test-uid"
    , stamp = start
    , time =
        Parser.WithTime
            { start = { posix = start, timeZoneName = Nothing }
            , end = Just { posix = end, timeZoneName = Nothing }
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
    , attendees = []
    , extraProperties = []
    }


occurrenceStartDateForFloating : Parser.Occurrence -> Date.Date
occurrenceStartDateForFloating occurrence =
    case occurrence.time of
        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year (intToMonth start.month) start.day

        _ ->
            occurrenceDate Time.utc occurrence


addRule : Recurrence.RecurrenceRule -> Parser.Event -> Parser.Event
addRule rule event =
    { event | recurrenceRules = rule :: event.recurrenceRules }


occurrenceDate : Time.Zone -> Parser.Occurrence -> Date.Date
occurrenceDate zone occ =
    case occ.time of
        Parser.WithTime { start } ->
            Date.fromPosix zone start.posix

        Parser.AllDay { start } ->
            start

        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year (intToMonth start.month) start.day


intToMonth : Int -> Time.Month
intToMonth m =
    case m of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec
