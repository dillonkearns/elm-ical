module VTimeZoneTests exposing (suite)

import ContentLine
import Date
import Expect
import Ical.Parser as Parser
import Iso8601
import Test exposing (..)
import Time
import VTimeZone


suite : Test
suite =
    describe "VTimeZone"
        [ offsetParsingTests
        , rruleTests
        , transitionDateTests
        , resolveTests
        , integrationTests
        ]


offsetParsingTests : Test
offsetParsingTests =
    describe "UTC offset parsing"
        [ test "parse -0500 as -18000 seconds" <|
            \() ->
                VTimeZone.parseOffset "-0500"
                    |> Expect.equal (Ok -18000)
        , test "parse +0530 as 19800 seconds" <|
            \() ->
                VTimeZone.parseOffset "+0530"
                    |> Expect.equal (Ok 19800)
        , test "parse +0000 as 0 seconds" <|
            \() ->
                VTimeZone.parseOffset "+0000"
                    |> Expect.equal (Ok 0)
        , test "parse -0400 as -14400 seconds" <|
            \() ->
                VTimeZone.parseOffset "-0400"
                    |> Expect.equal (Ok -14400)
        , test "parse +013045 as 5445 seconds" <|
            \() ->
                VTimeZone.parseOffset "+013045"
                    |> Expect.equal (Ok 5445)
        , test "invalid offset returns error" <|
            \() ->
                VTimeZone.parseOffset "invalid"
                    |> Expect.err
        ]


rruleTests : Test
rruleTests =
    describe "VTIMEZONE RRULE parsing"
        [ test "parse 2nd Sunday of March" <|
            \() ->
                VTimeZone.parseTransitionRule "FREQ=YEARLY;BYMONTH=3;BYDAY=2SU"
                    |> Expect.equal
                        (Ok { byMonth = 3, weekdayOrdinal = 2, weekday = Time.Sun })
        , test "parse 1st Sunday of November" <|
            \() ->
                VTimeZone.parseTransitionRule "FREQ=YEARLY;BYMONTH=11;BYDAY=1SU"
                    |> Expect.equal
                        (Ok { byMonth = 11, weekdayOrdinal = 1, weekday = Time.Sun })
        , test "parse last Sunday of October" <|
            \() ->
                VTimeZone.parseTransitionRule "FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU"
                    |> Expect.equal
                        (Ok { byMonth = 10, weekdayOrdinal = -1, weekday = Time.Sun })
        , test "parse last Friday of March" <|
            \() ->
                VTimeZone.parseTransitionRule "FREQ=YEARLY;BYMONTH=3;BYDAY=-1FR"
                    |> Expect.equal
                        (Ok { byMonth = 3, weekdayOrdinal = -1, weekday = Time.Fri })
        ]


transitionDateTests : Test
transitionDateTests =
    describe "transition date computation"
        [ test "2nd Sunday of March 2024 is March 10" <|
            \() ->
                VTimeZone.transitionDate 2024 { byMonth = 3, weekdayOrdinal = 2, weekday = Time.Sun }
                    |> Expect.equal (Date.fromCalendarDate 2024 Time.Mar 10)
        , test "1st Sunday of November 2024 is November 3" <|
            \() ->
                VTimeZone.transitionDate 2024 { byMonth = 11, weekdayOrdinal = 1, weekday = Time.Sun }
                    |> Expect.equal (Date.fromCalendarDate 2024 Time.Nov 3)
        , test "last Sunday of October 2024 is October 27" <|
            \() ->
                VTimeZone.transitionDate 2024 { byMonth = 10, weekdayOrdinal = -1, weekday = Time.Sun }
                    |> Expect.equal (Date.fromCalendarDate 2024 Time.Oct 27)
        , test "2nd Sunday of March 2026 is March 8" <|
            \() ->
                VTimeZone.transitionDate 2026 { byMonth = 3, weekdayOrdinal = 2, weekday = Time.Sun }
                    |> Expect.equal (Date.fromCalendarDate 2026 Time.Mar 8)
        ]


{-| America/New\_York VTIMEZONE data for testing.
-}
easternTimeZone : VTimeZone.ZoneDefinition
easternTimeZone =
    parseZone
        [ "TZID:America/New_York"
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
        ]


resolveTests : Test
resolveTests =
    describe "resolve local datetime to Posix"
        [ test "summer datetime in Eastern (EDT, -4h)" <|
            \() ->
                -- June 15, 2024 at 14:30:00 Eastern = 18:30:00 UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Jun, day = 15, hour = 14, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-06-15T18:30:00.000Z"))
        , test "winter datetime in Eastern (EST, -5h)" <|
            \() ->
                -- December 15, 2024 at 14:30:00 Eastern = 19:30:00 UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Dec, day = 15, hour = 14, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-12-15T19:30:00.000Z"))
        , test "just before spring-forward (still standard)" <|
            \() ->
                -- March 10, 2024 at 1:30 AM Eastern = 6:30 AM UTC (still EST, -5h)
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Mar, day = 10, hour = 1, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-03-10T06:30:00.000Z"))
        , test "after spring-forward (daylight)" <|
            \() ->
                -- March 10, 2024 at 3:30 AM Eastern = 7:30 AM UTC (EDT, -4h)
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Mar, day = 10, hour = 3, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-03-10T07:30:00.000Z"))
        , test "nonexistent spring-forward time uses offset before the gap" <|
            \() ->
                -- March 10, 2024 at 2:30 AM Eastern does not exist.
                -- RFC 5545 says to interpret it using the UTC offset before the gap,
                -- which resolves to 2024-03-10T07:30:00Z.
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Mar, day = 10, hour = 2, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-03-10T07:30:00.000Z"))
        , test "fall-back ambiguous time resolves to first occurrence (daylight)" <|
            \() ->
                -- Nov 3, 2024 at 1:30 AM Eastern — ambiguous!
                -- RFC 5545: "first occurrence" = still in daylight time (EDT, -4h)
                -- 1:30 AM EDT = 5:30 AM UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = Time.Nov, day = 3, hour = 1, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-11-03T05:30:00.000Z"))
        ]


integrationTests : Test
integrationTests =
    describe "end-to-end VTIMEZONE resolution"
        [ test "TZID datetime resolved to Posix via VTIMEZONE" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
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
                            , "UID:summer-meeting-1@test"
                            , "DTSTAMP:20240101T000000Z"
                            , "DTSTART;TZID=America/New_York:20240615T143000"
                            , "SUMMARY:Summer meeting"
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
                                            { start = { posix = toIso8601 "2024-06-15T18:30:00.000Z", timeZoneName = Just "America/New_York" }
                                            , end = Just { posix = toIso8601 "2024-06-15T18:30:00.000Z", timeZoneName = Just "America/New_York" }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "winter TZID datetime resolved correctly" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
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
                            , "UID:winter-meeting-1@test"
                            , "DTSTAMP:20240101T000000Z"
                            , "DTSTART;TZID=America/New_York:20241215T143000"
                            , "SUMMARY:Winter meeting"
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
                                            { start = { posix = toIso8601 "2024-12-15T19:30:00.000Z", timeZoneName = Just "America/New_York" }
                                            , end = Just { posix = toIso8601 "2024-12-15T19:30:00.000Z", timeZoneName = Just "America/New_York" }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        , test "historical VTIMEZONE rules are used for pre-2007 New York dates" <|
            \() ->
                let
                    input : String
                    input =
                        String.join "\u{000D}\n"
                            [ "BEGIN:VCALENDAR"
                            , "VERSION:2.0"
                            , "PRODID:-//test//EN"
                            , "BEGIN:VTIMEZONE"
                            , "TZID:America/New_York"
                            , "BEGIN:DAYLIGHT"
                            , "TZOFFSETFROM:-0500"
                            , "TZOFFSETTO:-0400"
                            , "DTSTART:19670430T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z"
                            , "END:DAYLIGHT"
                            , "BEGIN:DAYLIGHT"
                            , "TZOFFSETFROM:-0500"
                            , "TZOFFSETTO:-0400"
                            , "DTSTART:19760425T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z"
                            , "END:DAYLIGHT"
                            , "BEGIN:DAYLIGHT"
                            , "TZOFFSETFROM:-0500"
                            , "TZOFFSETTO:-0400"
                            , "DTSTART:19870405T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z"
                            , "END:DAYLIGHT"
                            , "BEGIN:DAYLIGHT"
                            , "TZOFFSETFROM:-0500"
                            , "TZOFFSETTO:-0400"
                            , "DTSTART:20070311T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU"
                            , "END:DAYLIGHT"
                            , "BEGIN:STANDARD"
                            , "TZOFFSETFROM:-0400"
                            , "TZOFFSETTO:-0500"
                            , "DTSTART:19671029T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z"
                            , "END:STANDARD"
                            , "BEGIN:STANDARD"
                            , "TZOFFSETFROM:-0400"
                            , "TZOFFSETTO:-0500"
                            , "DTSTART:20071104T020000"
                            , "RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU"
                            , "END:STANDARD"
                            , "END:VTIMEZONE"
                            , "BEGIN:VEVENT"
                            , "UID:historic-ny-1@test"
                            , "DTSTAMP:20240101T000000Z"
                            , "DTSTART;TZID=America/New_York:20061101T120000"
                            , "SUMMARY:Historic New York meeting"
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
                                            { start =
                                                { posix = toIso8601 "2006-11-01T17:00:00.000Z"
                                                , timeZoneName = Just "America/New_York"
                                                }
                                            , end =
                                                Just
                                                    { posix = toIso8601 "2006-11-01T17:00:00.000Z"
                                                    , timeZoneName = Just "America/New_York"
                                                    }
                                            }
                                        )

                            _ ->
                                Expect.fail "Expected 1 event"

                    Err err ->
                        Expect.fail err
        ]


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)


parseZone : List String -> VTimeZone.ZoneDefinition
parseZone lines =
    let
        contentLines : List ContentLine.ContentLine
        contentLines =
            lines
                |> List.map
                    (\line ->
                        case ContentLine.parse line of
                            Ok contentLine ->
                                contentLine

                            Err err ->
                                Debug.todo err
                    )
    in
    case VTimeZone.parseFromContentLines contentLines of
        Ok ( _, zone, [] ) ->
            zone

        Ok _ ->
            Debug.todo "Unexpected remaining content lines when parsing test timezone"

        Err err ->
            Debug.todo err
