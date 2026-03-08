module VTimeZoneTests exposing (suite)

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
        [ test "parse -0500 as -300 minutes" <|
            \() ->
                VTimeZone.parseOffset "-0500"
                    |> Expect.equal (Ok -300)
        , test "parse +0530 as 330 minutes" <|
            \() ->
                VTimeZone.parseOffset "+0530"
                    |> Expect.equal (Ok 330)
        , test "parse +0000 as 0 minutes" <|
            \() ->
                VTimeZone.parseOffset "+0000"
                    |> Expect.equal (Ok 0)
        , test "parse -0400 as -240 minutes" <|
            \() ->
                VTimeZone.parseOffset "-0400"
                    |> Expect.equal (Ok -240)
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
    { standardOffset = -300
    , standardTransition =
        Just
            { rule = { byMonth = 11, weekdayOrdinal = 1, weekday = Time.Sun }
            , transitionTime = { hour = 2, minute = 0, second = 0 }
            }
    , daylightOffset = -240
    , daylightTransition =
        Just
            { rule = { byMonth = 3, weekdayOrdinal = 2, weekday = Time.Sun }
            , transitionTime = { hour = 2, minute = 0, second = 0 }
            }
    }


resolveTests : Test
resolveTests =
    describe "resolve local datetime to Posix"
        [ test "summer datetime in Eastern (EDT, -4h)" <|
            \() ->
                -- June 15, 2024 at 14:30:00 Eastern = 18:30:00 UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = 6, day = 15, hour = 14, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-06-15T18:30:00.000Z"))
        , test "winter datetime in Eastern (EST, -5h)" <|
            \() ->
                -- December 15, 2024 at 14:30:00 Eastern = 19:30:00 UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = 12, day = 15, hour = 14, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-12-15T19:30:00.000Z"))
        , test "just before spring-forward (still standard)" <|
            \() ->
                -- March 10, 2024 at 1:30 AM Eastern = 6:30 AM UTC (still EST, -5h)
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = 3, day = 10, hour = 1, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-03-10T06:30:00.000Z"))
        , test "after spring-forward (daylight)" <|
            \() ->
                -- March 10, 2024 at 3:30 AM Eastern = 7:30 AM UTC (EDT, -4h)
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = 3, day = 10, hour = 3, minute = 30, second = 0 }
                    |> Expect.equal (Ok (toIso8601 "2024-03-10T07:30:00.000Z"))
        , test "fall-back ambiguous time resolves to first occurrence (daylight)" <|
            \() ->
                -- Nov 3, 2024 at 1:30 AM Eastern — ambiguous!
                -- RFC 5545: "first occurrence" = still in daylight time (EDT, -4h)
                -- 1:30 AM EDT = 5:30 AM UTC
                VTimeZone.resolve easternTimeZone
                    { year = 2024, month = 11, day = 3, hour = 1, minute = 30, second = 0 }
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
                                        (Parser.WithTime { start = { posix = toIso8601 "2024-06-15T18:30:00.000Z", timeZoneName = Just "America/New_York" }, end = Nothing })

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
                                        (Parser.WithTime { start = { posix = toIso8601 "2024-12-15T19:30:00.000Z", timeZoneName = Just "America/New_York" }, end = Nothing })

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
