module ParseTests exposing (suite)

import ContentLine
import Date
import Expect
import Fuzz
import Ical
import Ical.Parser as Parser
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
                    |> Expect.equal (Ok (Just "2.0"))
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
                            , "END:VEVENT"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 2"
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
                                    , \e -> e.uid |> Expect.equal (Just "alarm-test")
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
                            , \c -> c.version |> Expect.equal (Just "2.0")
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
                                    , \e -> e.uid |> Expect.equal (Just "test-1@example.com")
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
                            , "END:VEVENT"
                            , "BEGIN:VEVENT"
                            , "SUMMARY:Event 2"
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just (Parser.DateOnly { year = 2021, month = 3, day = 18 }))

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
                                            { calAddress = "mailto:dillon@example.com"
                                            , commonName = Just "Dillon Kearns"
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
                                ev.properties
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just
                                            (Parser.DateTime
                                                { year = 1997
                                                , month = 7
                                                , day = 14
                                                , hour = 13
                                                , minute = 30
                                                , second = 0
                                                }
                                                (Parser.Tzid "America/New_York")
                                            )
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
                                    [ \e -> e.status |> Expect.equal (Just "CONFIRMED")
                                    , \e -> e.transp |> Expect.equal (Just "TRANSPARENT")
                                    ]
                                    ev

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "UTC datetime has Utc timezone" <|
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just
                                            (Parser.DateTime
                                                { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                                Parser.Utc
                                            )
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just
                                            (Parser.DateTime
                                                { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                                Parser.Utc
                                            )
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just
                                            (Parser.DateTime
                                                { year = 1997, month = 7, day = 14, hour = 13, minute = 30, second = 0 }
                                                (Parser.Tzid "America/Chicago")
                                            )
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "RRULE preserved in event properties" <|
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
                                ev.properties
                                    |> List.filter (\p -> p.name == "RRULE")
                                    |> List.map .value
                                    |> Expect.equal [ "FREQ=WEEKLY;BYDAY=MO,WE,FR" ]

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
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
                                ev.dtstart
                                    |> Expect.equal
                                        (Just
                                            (Parser.DateTime
                                                { year = 2021, month = 3, day = 18, hour = 16, minute = 20, second = 44 }
                                                Parser.Floating
                                            )
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
                                    , \e -> e.uid |> Expect.equal (Just "rt-1@test.com")
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
                                    , \e -> e.dtstart |> Expect.equal (Just (Parser.DateOnly { year = 2021, month = 3, day = 18 }))
                                    , \e -> e.dtend |> Expect.equal (Just (Parser.DateOnly { year = 2021, month = 3, day = 19 }))
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
                                            [ \o -> o.calAddress |> Expect.equal "mailto:dillon@example.com"
                                            , \o -> o.commonName |> Expect.equal (Just "Dillon Kearns")
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
                            [ \c -> c.version |> Expect.equal (Just "2.0")
                            , \c -> c.prodId |> Expect.equal (Just "-//myapp//cal//EN")
                            ]
                            cal

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
