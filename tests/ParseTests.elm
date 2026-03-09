module ParseTests exposing (suite)

import ContentLine
import Date
import Expect
import Fuzz
import Ical
import Ical.Parser as Parser
import Ical.Recurrence as Recurrence
import Iso8601
import Test exposing (..)
import Time
import ValueParser


suite : Test
suite =
    describe "Parsing"
        [ unfoldTests
        , contentLineTests
        , valueParserTests
        , recurrenceRuleTests
        , componentTests
        , endToEndTests
        , roundTripTests
        ]



-- Phase A: Line Unfolding


unfoldTests : Test
unfoldTests =
    describe "line unfolding"
        [ test "simple unfolded line passthrough" <|
            \() ->
                ContentLine.unfold "SUMMARY:hello"
                    |> Expect.equal "SUMMARY:hello"
        , test "CRLF + space continuation joins lines" <|
            \() ->
                ContentLine.unfold "SUMMARY:hel\u{000D}\n lo"
                    |> Expect.equal "SUMMARY:hello"
        , test "CRLF + tab continuation joins lines" <|
            \() ->
                ContentLine.unfold "SUMMARY:hel\u{000D}\n\tlo"
                    |> Expect.equal "SUMMARY:hello"
        , test "LF-only continuation" <|
            \() ->
                ContentLine.unfold "SUMMARY:hel\n lo"
                    |> Expect.equal "SUMMARY:hello"
        , test "multiple consecutive folded lines" <|
            \() ->
                ContentLine.unfold "DESC:aa\u{000D}\n bb\u{000D}\n cc"
                    |> Expect.equal "DESC:aabbcc"
        ]



-- Phase B: Content Line Parsing


contentLineTests : Test
contentLineTests =
    describe "content line parsing"
        [ test "simple property" <|
            \() ->
                ContentLine.parse "SUMMARY:hello"
                    |> Expect.equal
                        (Ok
                            { name = "SUMMARY"
                            , parameters = []
                            , value = "hello"
                            }
                        )
        , test "property with parameter" <|
            \() ->
                ContentLine.parse "DTSTART;VALUE=DATE:20210318"
                    |> Expect.equal
                        (Ok
                            { name = "DTSTART"
                            , parameters = [ ( "VALUE", "DATE" ) ]
                            , value = "20210318"
                            }
                        )
        , test "property with quoted parameter" <|
            \() ->
                ContentLine.parse "ORGANIZER;CN=\"Smith, John\":mailto:john@example.com"
                    |> Expect.equal
                        (Ok
                            { name = "ORGANIZER"
                            , parameters = [ ( "CN", "Smith, John" ) ]
                            , value = "mailto:john@example.com"
                            }
                        )
        , test "property with multiple parameters" <|
            \() ->
                ContentLine.parse "DTSTART;TZID=America/New_York;VALUE=DATE-TIME:19970714T133000"
                    |> Expect.equal
                        (Ok
                            { name = "DTSTART"
                            , parameters = [ ( "TZID", "America/New_York" ), ( "VALUE", "DATE-TIME" ) ]
                            , value = "19970714T133000"
                            }
                        )
        , test "empty value" <|
            \() ->
                ContentLine.parse "DESCRIPTION:"
                    |> Expect.equal
                        (Ok
                            { name = "DESCRIPTION"
                            , parameters = []
                            , value = ""
                            }
                        )
        , test "value containing colons" <|
            \() ->
                ContentLine.parse "URL:https://example.com"
                    |> Expect.equal
                        (Ok
                            { name = "URL"
                            , parameters = []
                            , value = "https://example.com"
                            }
                        )
        , test "quoted parameter containing colon" <|
            \() ->
                ContentLine.parse "ATTENDEE;CN=\"Room: 101\":mailto:room@example.com"
                    |> Expect.equal
                        (Ok
                            { name = "ATTENDEE"
                            , parameters = [ ( "CN", "Room: 101" ) ]
                            , value = "mailto:room@example.com"
                            }
                        )
        , test "quoted parameter containing semicolon" <|
            \() ->
                ContentLine.parse "ORGANIZER;CN=\"Smith; John\":mailto:john@example.com"
                    |> Expect.equal
                        (Ok
                            { name = "ORGANIZER"
                            , parameters = [ ( "CN", "Smith; John" ) ]
                            , value = "mailto:john@example.com"
                            }
                        )
        , test "lowercase property name is uppercased" <|
            \() ->
                ContentLine.parse "summary:hello"
                    |> Expect.equal
                        (Ok
                            { name = "SUMMARY"
                            , parameters = []
                            , value = "hello"
                            }
                        )
        , test "RRULE-style value with semicolons" <|
            \() ->
                ContentLine.parse "RRULE:FREQ=WEEKLY;BYDAY=MO,TU,WE"
                    |> Expect.equal
                        (Ok
                            { name = "RRULE"
                            , parameters = []
                            , value = "FREQ=WEEKLY;BYDAY=MO,TU,WE"
                            }
                        )
        ]



-- Phase D: Value Parsing


valueParserTests : Test
valueParserTests =
    describe "value parsing"
        [ test "parse DATE" <|
            \() ->
                ValueParser.parseDate "20210318"
                    |> Expect.equal
                        (Ok { year = 2021, month = 3, day = 18 })
        , test "reject DATE with out-of-range month/day" <|
            \() ->
                ValueParser.parseDate "20211340"
                    |> Expect.err
        , test "parse DATE-TIME with Z" <|
            \() ->
                ValueParser.parseDateTime "20210318T162044Z"
                    |> Expect.equal
                        (Ok
                            { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44, isUtc = True }
                        )
        , test "parse DATE-TIME without Z" <|
            \() ->
                ValueParser.parseDateTime "20210318T162044"
                    |> Expect.equal
                        (Ok
                            { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44, isUtc = False }
                        )
        , test "reject DATE-TIME with out-of-range time" <|
            \() ->
                ValueParser.parseDateTime "20210318T256199Z"
                    |> Expect.err
        , test "TEXT unescaping" <|
            \() ->
                ValueParser.unescapeText "hello\\nworld\\\\foo\\,bar\\;baz"
                    |> Expect.equal "hello\nworld\\foo,bar;baz"
        , test "unknown escape sequence strips backslash" <|
            \() ->
                ValueParser.unescapeText "hello\\aworld"
                    |> Expect.equal "helloaworld"
        , test "uppercase N escape also produces newline" <|
            \() ->
                ValueParser.unescapeText "line1\\Nline2"
                    |> Expect.equal "line1\nline2"
        , test "parse DURATION PT1H30M" <|
            \() ->
                ValueParser.parseDuration "PT1H30M"
                    |> Expect.equal (Ok { weeks = 0, days = 0, hours = 1, minutes = 30, seconds = 0 })
        , test "parse DURATION P1D" <|
            \() ->
                ValueParser.parseDuration "P1D"
                    |> Expect.equal (Ok { weeks = 0, days = 1, hours = 0, minutes = 0, seconds = 0 })
        , test "parse DURATION P1W" <|
            \() ->
                ValueParser.parseDuration "P1W"
                    |> Expect.equal (Ok { weeks = 1, days = 0, hours = 0, minutes = 0, seconds = 0 })
        , test "parse DURATION P1DT2H30M" <|
            \() ->
                ValueParser.parseDuration "P1DT2H30M"
                    |> Expect.equal (Ok { weeks = 0, days = 1, hours = 2, minutes = 30, seconds = 0 })
        , test "parse DURATION PT15M" <|
            \() ->
                ValueParser.parseDuration "PT15M"
                    |> Expect.equal (Ok { weeks = 0, days = 0, hours = 0, minutes = 15, seconds = 0 })
        , test "parse DURATION PT30S" <|
            \() ->
                ValueParser.parseDuration "PT30S"
                    |> Expect.equal (Ok { weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 30 })
        , test "invalid DURATION returns error" <|
            \() ->
                ValueParser.parseDuration "invalid"
                    |> Expect.err
        , test "malformed DURATION tokens return error" <|
            \() ->
                ValueParser.parseDuration "PTXM"
                    |> Expect.err
        ]


recurrenceRuleTests : Test
recurrenceRuleTests =
    describe "recurrence rule parsing"
        [ test "parse FREQ=DAILY" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=DAILY"
                    |> Expect.equal
                        (Ok
                            { frequency = Recurrence.Daily
                            , interval = 1
                            , end = Recurrence.Forever
                            , byDay = []
                            , byMonthDay = []
                            , byMonth = []
                            , bySetPos = []
                            , weekStart = Time.Mon
                            }
                        )
        , test "parse FREQ=WEEKLY with BYDAY" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=WEEKLY;BYDAY=MO,WE,FR"
                    |> Expect.equal
                        (Ok
                            { frequency = Recurrence.Weekly
                            , interval = 1
                            , end = Recurrence.Forever
                            , byDay =
                                [ { ordinal = Nothing, weekday = Time.Mon }
                                , { ordinal = Nothing, weekday = Time.Wed }
                                , { ordinal = Nothing, weekday = Time.Fri }
                                ]
                            , byMonthDay = []
                            , byMonth = []
                            , bySetPos = []
                            , weekStart = Time.Mon
                            }
                        )
        , test "parse with INTERVAL" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=DAILY;INTERVAL=2"
                    |> Result.map .interval
                    |> Expect.equal (Ok 2)
        , test "invalid INTERVAL returns error" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=DAILY;INTERVAL=foo"
                    |> Expect.err
        , test "parse with COUNT" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=WEEKLY;COUNT=10"
                    |> Result.map .end
                    |> Expect.equal (Ok (Recurrence.Count 10))
        , test "parse with UNTIL date" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=DAILY;UNTIL=20210401"
                    |> Result.map .end
                    |> Expect.equal (Ok (Recurrence.UntilDate (Date.fromCalendarDate 2021 Time.Apr 1)))
        , test "parse with UNTIL datetime" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=DAILY;UNTIL=20210401T000000Z"
                    |> Result.map .end
                    |> Expect.equal (Ok (Recurrence.UntilDateTime (toIso8601 "2021-04-01T00:00:00.000Z")))
        , test "parse MONTHLY with BYMONTHDAY" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=MONTHLY;BYMONTHDAY=15"
                    |> Result.map .byMonthDay
                    |> Expect.equal (Ok [ 15 ])
        , test "parse YEARLY with BYMONTH" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=YEARLY;BYMONTH=3,6,9,12"
                    |> Result.map .byMonth
                    |> Expect.equal (Ok [ 3, 6, 9, 12 ])
        , test "parse ordinal BYDAY (2nd Sunday)" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=MONTHLY;BYDAY=2SU"
                    |> Result.map .byDay
                    |> Expect.equal (Ok [ { ordinal = Just 2, weekday = Time.Sun } ])
        , test "parse negative ordinal BYDAY (last Friday)" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=MONTHLY;BYDAY=-1FR"
                    |> Result.map .byDay
                    |> Expect.equal (Ok [ { ordinal = Just -1, weekday = Time.Fri } ])
        , test "parse with WKST" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=WEEKLY;WKST=SU"
                    |> Result.map .weekStart
                    |> Expect.equal (Ok Time.Sun)
        , test "invalid WKST returns error" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=WEEKLY;WKST=XX"
                    |> Expect.err
        , test "invalid BYMONTH returns error" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=YEARLY;BYMONTH=13"
                    |> Expect.err
        , test "parse complex rule" <|
            \() ->
                ValueParser.parseRecurrenceRule "FREQ=MONTHLY;INTERVAL=2;BYDAY=1MO;COUNT=6;WKST=SU"
                    |> Expect.equal
                        (Ok
                            { frequency = Recurrence.Monthly
                            , interval = 2
                            , end = Recurrence.Count 6
                            , byDay = [ { ordinal = Just 1, weekday = Time.Mon } ]
                            , byMonthDay = []
                            , byMonth = []
                            , bySetPos = []
                            , weekStart = Time.Sun
                            }
                        )
        , test "missing FREQ returns error" <|
            \() ->
                ValueParser.parseRecurrenceRule "INTERVAL=2;BYDAY=MO"
                    |> Expect.err
        ]



-- Phase C: Component Structure


componentTests : Test
componentTests =
    describe "component structure"
        [ test "parse VCALENDAR with properties" <|
            \() ->
                let
                    input : String
                    input =
                        "BEGIN:VCALENDAR\u{000D}\nVERSION:2.0\u{000D}\nPRODID:-//test//EN\u{000D}\nEND:VCALENDAR\u{000D}\n"
                in
                Parser.parse input
                    |> Result.map .version
                    |> Expect.equal (Ok "2.0")
        , test "nested VEVENT inside VCALENDAR" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Test Event"
                            , "UID:test-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Result.map (.events >> List.length)
                    |> Expect.equal (Ok 1)
        , test "multiple VEVENT children" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 1"
                            , "UID:event-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "END:VEVENT"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 2"
                            , "UID:event-2"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Result.map (.events >> List.length)
                    |> Expect.equal (Ok 2)
        , test "VALARM nested inside VEVENT is skipped" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event with alarm"
                            , "UID:alarm-test"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "BEGIN:VALARM"
                            , "TRIGGER:-PT15M"
                            , "ACTION:DISPLAY"
                            , "DESCRIPTION:Reminder"
                            , "END:VALARM"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.summary |> Expect.equal (Just "Event with alarm")
                                    , \e -> e.uid |> Expect.equal "alarm-test"
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail ("Parse failed: " ++ err)
        , test "properties after nested VALARM are still parsed" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Before alarm"
                            , "UID:before-alarm"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "BEGIN:VALARM"
                            , "TRIGGER:-PT15M"
                            , "ACTION:DISPLAY"
                            , "END:VALARM"
                            , "DESCRIPTION:After alarm"
                            , "LOCATION:Room 42"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.summary |> Expect.equal (Just "Before alarm")
                                    , \e -> e.description |> Expect.equal (Just "After alarm")
                                    , \e -> e.location |> Expect.equal (Just "Room 42")
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail ("Parse failed: " ++ err)
        , test "empty calendar with no events" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        Expect.all
                            [ \c -> c.events |> Expect.equal []
                            , \c -> c.version |> Expect.equal "2.0"
                            ]
                            cal

                    Err err ->
                        Expect.fail err
        , test "mismatched BEGIN/END gives error" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "END:VEVENT"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "malformed content line returns error instead of being ignored" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "UID:test-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "BAD!LINE:boom"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        ]



-- Phase E: End-to-End Parsing


endToEndTests : Test
endToEndTests =
    describe "end-to-end parsing"
        [ test "parse simple complete calendar" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "DTEND:20210318T172044Z"
                            , "DTSTAMP:20210318T162044Z"
                            , "UID:test-1@example.com"
                            , "SUMMARY:Test Event"
                            , "DESCRIPTION:A test event"
                            , "LOCATION:Room 101"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.summary |> Expect.equal (Just "Test Event")
                                    , \e -> e.description |> Expect.equal (Just "A test event")
                                    , \e -> e.location |> Expect.equal (Just "Room 101")
                                    , \e -> e.uid |> Expect.equal "test-1@example.com"
                                    ]
                                    ev

                            other ->
                                Expect.fail ("Expected 1 event, got " ++ String.fromInt (List.length other))

                    Err err ->
                        Expect.fail err
        , test "parse calendar with multiple events" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 1"
                            , "UID:event-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "END:VEVENT"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 2"
                            , "UID:event-2"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        cal.events
                            |> List.map .summary
                            |> Expect.equal [ Just "Event 1", Just "Event 2" ]

                    Err err ->
                        Expect.fail err
        , test "parse all-day event" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;VALUE=DATE:20210318"
                            , "DTEND;VALUE=DATE:20210319"
                            , "UID:allday-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:All day"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.AllDay
                                            { start = Date.fromCalendarDate 2021 Time.Mar 18
                                            , end = Just (Date.fromCalendarDate 2021 Time.Mar 19)
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "all-day DTSTART without DTEND defaults to one day" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;VALUE=DATE:20210318"
                            , "UID:allday-default-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:All day default"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.AllDay
                                            { start = Date.fromCalendarDate 2021 Time.Mar 18
                                            , end = Just (Date.fromCalendarDate 2021 Time.Mar 19)
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "parse event with organizer" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Meeting"
                            , "UID:organizer-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "ORGANIZER;CN=\"Dillon Kearns\":mailto:dillon@example.com"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.organizer
                                    |> Expect.equal
                                        (Just
                                            { email = "dillon@example.com"
                                            , name = Just "Dillon Kearns"
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "parse calendar with folded long description" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Test"
                            , "UID:folded-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed "
                            , " diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"
                            , " \\, sed diam voluptua.\\nbeep boop"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.description
                                    |> Expect.equal
                                        (Just "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop")

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "unknown properties preserved" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Test"
                            , "UID:custom-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "X-CUSTOM-PROP:custom value"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.extraProperties
                                    |> List.filter (\p -> p.name == "X-CUSTOM-PROP")
                                    |> List.map .value
                                    |> Expect.equal [ "custom value" ]

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "parse event with TZID parameter on DTSTART" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;TZID=America/New_York:19970714T133000"
                            , "UID:tzid-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Meeting"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.FloatingTime
                                            { start =
                                                { year = 1997
                                                , month = 7
                                                , day = 14
                                                , hour = 13
                                                , minute = 30
                                                , second = 0
                                                }
                                            , end =
                                                Just
                                                    { year = 1997
                                                    , month = 7
                                                    , day = 14
                                                    , hour = 13
                                                    , minute = 30
                                                    , second = 0
                                                    }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "parse event with status and transparency" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Test"
                            , "UID:status-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "STATUS:CONFIRMED"
                            , "TRANSP:TRANSPARENT"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.status |> Expect.equal (Just Parser.Confirmed)
                                    , \e -> e.transparency |> Expect.equal (Just Parser.Transparent)
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "UTC datetime becomes Time.Posix" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "UID:utc-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.WithTime
                                            { start = { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            , end = Just { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "date-time DTSTART without DTEND defaults to same instant" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "UID:utc-default-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Instant event"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.WithTime
                                            { start = { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            , end = Just { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "explicit VALUE=DATE-TIME parameter works" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;VALUE=DATE-TIME:20210318T162044Z"
                            , "UID:date-time-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.WithTime
                                            { start = { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            , end = Just { posix = toIso8601 "2021-03-18T16:20:44.000Z", timeZoneName = Nothing }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "lowercase parameter names are matched case-insensitively" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;tzid=America/Chicago:19970714T133000"
                            , "UID:lowercase-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.FloatingTime
                                            { start = { year = 1997, month = 7, day = 14, hour = 13, minute = 30, second = 0 }
                                            , end = Just { year = 1997, month = 7, day = 14, hour = 13, minute = 30, second = 0 }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "invalid DTSTART value fails parsing" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20211318T162044Z"
                            , "UID:bad-start"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "invalid DTEND value fails parsing" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "DTEND:not-a-date"
                            , "UID:bad-end"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "VEVENT with both DTEND and DURATION fails parsing" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "DTEND:20210318T172044Z"
                            , "DURATION:PT1H"
                            , "UID:bad-both"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "invalid RRULE value fails parsing" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044Z"
                            , "UID:bad-rrule"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "RRULE:FREQ=DAILY;INTERVAL=foo"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "RRULE parsed into structured recurrenceRules" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Weekly meeting"
                            , "UID:rrule-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.recurrenceRules
                                    |> Expect.equal
                                        [ { frequency = Recurrence.Weekly
                                          , interval = 1
                                          , end = Recurrence.Forever
                                          , byDay =
                                                [ { ordinal = Nothing, weekday = Time.Mon }
                                                , { ordinal = Nothing, weekday = Time.Wed }
                                                , { ordinal = Nothing, weekday = Time.Fri }
                                                ]
                                          , byMonthDay = []
                                          , byMonth = []
                                          , bySetPos = []
                                          , weekStart = Time.Mon
                                          }
                                        ]

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "EXDATE parsed into exclusions list" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Weekly meeting"
                            , "UID:exdate-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "RRULE:FREQ=WEEKLY;BYDAY=MO"
                            , "EXDATE:20210325T162044Z,20210401T162044Z"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.exclusions
                                    |> Expect.equal
                                        [ toIso8601 "2021-03-25T16:20:44.000Z"
                                        , toIso8601 "2021-04-01T16:20:44.000Z"
                                        ]

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "ATTENDEE parsed into attendees list" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Team meeting"
                            , "UID:attendee-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "DTSTART:20210318T162044Z"
                            , "ATTENDEE;CN=\"Jane Smith\";ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE:mailto:jane@example.com"
                            , "ATTENDEE;CN=\"Bob Jones\":mailto:bob@example.com"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e ->
                                        e.attendees
                                            |> List.length
                                            |> Expect.equal 2
                                    , \e ->
                                        e.attendees
                                            |> List.map .email
                                            |> Expect.equal [ "jane@example.com", "bob@example.com" ]
                                    , \e ->
                                        e.attendees
                                            |> List.map .name
                                            |> Expect.equal [ Just "Jane Smith", Just "Bob Jones" ]
                                    , \e ->
                                        e.attendees
                                            |> List.head
                                            |> Maybe.map .role
                                            |> Expect.equal (Just Parser.Required)
                                    , \e ->
                                        e.attendees
                                            |> List.head
                                            |> Maybe.map .participationStatus
                                            |> Expect.equal (Just Parser.Accepted)
                                    , \e ->
                                        e.attendees
                                            |> List.head
                                            |> Maybe.map .rsvp
                                            |> Expect.equal (Just True)
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "DURATION resolved into end time (UTC)" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T100000Z"
                            , "DURATION:PT1H30M"
                            , "UID:duration-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Duration test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.WithTime
                                            { start = { posix = toIso8601 "2021-03-18T10:00:00.000Z", timeZoneName = Nothing }
                                            , end = Just { posix = toIso8601 "2021-03-18T11:30:00.000Z", timeZoneName = Nothing }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "DURATION resolved into end time (all-day)" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;VALUE=DATE:20210318"
                            , "DURATION:P2D"
                            , "UID:duration-2"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Multi-day duration"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.AllDay
                                            { start = Date.fromCalendarDate 2021 Time.Mar 18
                                            , end = Just (Date.fromCalendarDate 2021 Time.Mar 20)
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "DTSTART+DTEND type mismatch gives error" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART;VALUE=DATE:20210318"
                            , "DTEND:20210319T100000Z"
                            , "UID:mismatch-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Mismatch"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                Parser.parse input
                    |> Expect.err
        , test "local datetime has Floating timezone" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044"
                            , "UID:floating-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Test"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.FloatingTime
                                            { start = { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                            , end = Just { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "floating DTSTART without DTEND defaults to same local instant" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VEVENT"
                            , "DTSTART:20210318T162044"
                            , "UID:floating-default-1"
                            , "DTSTAMP:20210318T162044Z"
                            , "SUMMARY:Floating instant"
                            , "END:VEVENT"
                            , "END:VCALENDAR"
                            , ""
                            ]
                in
                case Parser.parse input of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.time
                                    |> Expect.equal
                                        (Parser.FloatingTime
                                            { start = { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                            , end = Just { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        ]



-- Phase F: Round-Trip Tests


roundTripTests : Test
roundTripTests =
    describe "round-trip"
        [ test "generate then parse recovers summary and description" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-1"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Round-trip test"
                            }
                            |> Ical.withDescription "A description with, commas; semicolons\nand newlines"
                            |> Ical.withLocation "Room 42"
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.summary |> Expect.equal (Just "Round-trip test")
                                    , \e -> e.description |> Expect.equal (Just "A description with, commas; semicolons\nand newlines")
                                    , \e -> e.location |> Expect.equal (Just "Room 42")
                                    , \e -> e.uid |> Expect.equal "rt-1@test.com"
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "round-trip all-day event" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-allday"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.AllDay
                                    { start = Date.fromCalendarDate 2021 Time.Mar 18
                                    , end = Date.fromCalendarDate 2021 Time.Mar 18
                                    }
                            , summary = "All day round-trip"
                            }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.summary |> Expect.equal (Just "All day round-trip")
                                    , \e ->
                                        e.time
                                            |> Expect.equal
                                                (Parser.AllDay
                                                    { start = Date.fromCalendarDate 2021 Time.Mar 18
                                                    , end = Just (Date.fromCalendarDate 2021 Time.Mar 19)
                                                    }
                                                )
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "round-trip event with organizer" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-org"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Organizer test"
                            }
                            |> Ical.withOrganizer { name = "Dillon Kearns", email = "dillon@example.com" }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                case ev.organizer of
                                    Just org ->
                                        Expect.all
                                            [ \o -> o.email |> Expect.equal "dillon@example.com"
                                            , \o -> o.name |> Expect.equal (Just "Dillon Kearns")
                                            ]
                                            org

                                    Nothing ->
                                        Expect.fail "Expected organizer"

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "round-trip long description with line folding" <|
            \() ->
                let
                    longDesc : String
                    longDesc =
                        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop"

                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-long"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Long description test"
                            }
                            |> Ical.withDescription longDesc
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                ev.description
                                    |> Expect.equal (Just longDesc)

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "round-trip calendar-level properties" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-cal"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Test"
                            }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//myapp//cal//EN"
                                    , domain = "test.com"
                                    }
                                    |> Ical.withName "My Calendar"
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        Expect.all
                            [ \c -> c.version |> Expect.equal "2.0"
                            , \c -> c.prodId |> Expect.equal "-//myapp//cal//EN"
                            ]
                            cal

                    Err err ->
                        Expect.fail err
        , test "round-trip event with RRULE" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-rrule"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Weekly standup"
                            }
                            |> Ical.withRecurrenceRule
                                { frequency = Recurrence.Weekly
                                , interval = 1
                                , end = Recurrence.Count 10
                                , byDay = [ { ordinal = Nothing, weekday = Time.Mon }, { ordinal = Nothing, weekday = Time.Wed } ]
                                , byMonthDay = []
                                , byMonth = []
                                , bySetPos = []
                                , weekStart = Time.Mon
                                }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                case ev.recurrenceRules of
                                    [ rule ] ->
                                        Expect.all
                                            [ \r -> r.frequency |> Expect.equal Recurrence.Weekly
                                            , \r -> r.end |> Expect.equal (Recurrence.Count 10)
                                            , \r -> r.byDay |> List.map .weekday |> Expect.equal [ Time.Mon, Time.Wed ]
                                            ]
                                            rule

                                    _ ->
                                        Expect.fail ("Expected 1 recurrence rule, got " ++ String.fromInt (List.length ev.recurrenceRules))

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "round-trip event with ATTENDEE" <|
            \() ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "rt-attendee"
                            , stamp = toIso8601 "2021-03-18T16:20:44.000Z"
                            , time =
                                Ical.WithTime
                                    { start = toIso8601 "2021-03-18T10:00:00.000Z"
                                    , end = toIso8601 "2021-03-18T11:00:00.000Z"
                                    }
                            , summary = "Meeting with attendees"
                            }
                            |> Ical.withAttendee { name = "Alice", email = "alice@example.com" }
                            |> Ical.withAttendee { name = "Bob", email = "bob@example.com" }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        case cal.events of
                            [ ev ] ->
                                Expect.all
                                    [ \e -> e.attendees |> List.length |> Expect.equal 2
                                    , \e -> e.attendees |> List.map .email |> Expect.equal [ "alice@example.com", "bob@example.com" ]
                                    , \e -> e.attendees |> List.map .name |> Expect.equal [ Just "Alice", Just "Bob" ]
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , fuzz Fuzz.string "fuzz round-trip preserves summary" <|
            \randomSummary ->
                let
                    summary : String
                    summary =
                        if String.isEmpty randomSummary then
                            "fallback"

                        else
                            randomSummary

                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "fuzz-1"
                            , stamp = Time.millisToPosix 1616083244000
                            , time =
                                Ical.WithTime
                                    { start = Time.millisToPosix 1616083244000
                                    , end = Time.millisToPosix 1616086844000
                                    }
                            , summary = summary
                            }
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )
                in
                case Parser.parse icsString of
                    Ok cal ->
                        cal.events
                            |> List.head
                            |> Maybe.andThen .summary
                            |> Expect.equal (Just (normalizeNewlines summary))

                    Err err ->
                        Expect.fail err
        , fuzz Fuzz.string "fuzz round-trip preserves description" <|
            \randomDesc ->
                let
                    icsString : String
                    icsString =
                        [ Ical.event
                            { id = "fuzz-2"
                            , stamp = Time.millisToPosix 1616083244000
                            , time =
                                Ical.WithTime
                                    { start = Time.millisToPosix 1616083244000
                                    , end = Time.millisToPosix 1616086844000
                                    }
                            , summary = "Fuzz test"
                            }
                            |> Ical.withDescription randomDesc
                        ]
                            |> Ical.generate
                                (Ical.config
                                    { id = "//test//test//EN"
                                    , domain = "test.com"
                                    }
                                )

                    normalized : String
                    normalized =
                        normalizeNewlines randomDesc

                    expected : Maybe String
                    expected =
                        if String.isEmpty normalized then
                            Nothing

                        else
                            Just normalized
                in
                case Parser.parse icsString of
                    Ok cal ->
                        cal.events
                            |> List.head
                            |> Maybe.andThen .description
                            |> Expect.equal expected

                    Err err ->
                        Expect.fail err
        ]


{-| iCal normalizes all newline variants (CRLF, CR, LF) to escaped \\n,
so round-tripping collapses them to LF.
-}
normalizeNewlines : String -> String
normalizeNewlines =
    String.replace "\u{000D}\n" "\n"
        >> String.replace "\u{000D}" "\n"


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)
