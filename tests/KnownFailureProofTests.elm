module KnownFailureProofTests exposing (suite)

import Date
import Expect
import Ical
import Ical.Parser as Parser
import Ical.Recurrence as Recurrence
import Iso8601
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Known failure proofs"
        [ describe "recurrence identity and time precision"
            [ test "timed RDATE should preserve its explicit time-of-day during expansion" <|
                \() ->
                    let
                        input : String
                        input =
                            calendar
                                [ "BEGIN:VEVENT"
                                , "UID:rdate-time@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T100000Z"
                                , "DTEND:20240101T110000Z"
                                , "RDATE:20240102T140000Z"
                                , "SUMMARY:RDATE explicit time"
                                , "END:VEVENT"
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 3
                                }
                                cal.events
                                |> List.map occurrenceStartMillis
                                |> Expect.equal
                                    [ toMillis "2024-01-01T10:00:00.000Z"
                                    , toMillis "2024-01-02T14:00:00.000Z"
                                    ]

                        Err err ->
                            Expect.fail err
            , test "EXDATE should exclude only the targeted timed occurrence, not the whole day" <|
                \() ->
                    let
                        input : String
                        input =
                            calendar
                                [ "BEGIN:VEVENT"
                                , "UID:exdate-one-slot@test"
                                , "DTSTAMP:20240101T000000Z"
                                , "DTSTART:20240101T090000Z"
                                , "DTEND:20240101T100000Z"
                                , "RRULE:FREQ=DAILY;COUNT=4;BYHOUR=9,10"
                                , "EXDATE:20240102T100000Z"
                                , "SUMMARY:Two slots per day"
                                , "END:VEVENT"
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 2
                                }
                                cal.events
                                |> List.map occurrenceStartMillis
                                |> Expect.equal
                                    [ toMillis "2024-01-01T09:00:00.000Z"
                                    , toMillis "2024-01-01T10:00:00.000Z"
                                    , toMillis "2024-01-02T09:00:00.000Z"
                                    ]

                        Err err ->
                            Expect.fail err
            , test "RECURRENCE-ID override should replace only the targeted timed occurrence" <|
                \() ->
                    let
                        input : String
                        input =
                            calendar
                                [ "BEGIN:VEVENT"
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
                                ]
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 2
                                }
                                cal.events
                                |> List.map
                                    (\occ ->
                                        ( occurrenceStartMillis occ
                                        , Maybe.withDefault "" occ.event.summary
                                        )
                                    )
                                |> Expect.equal
                                    [ ( toMillis "2024-01-01T09:00:00.000Z", "Original slot" )
                                    , ( toMillis "2024-01-01T10:00:00.000Z", "Original slot" )
                                    , ( toMillis "2024-01-02T09:00:00.000Z", "Original slot" )
                                    , ( toMillis "2024-01-02T15:00:00.000Z", "Moved 10am slot" )
                                    ]

                        Err err ->
                            Expect.fail err
            , test "TZID recurrence near midnight should follow the local weekday, not the UTC day" <|
                \() ->
                    let
                        input : String
                        input =
                            calendar
                                (easternTimeZone
                                    ++ [ "BEGIN:VEVENT"
                                       , "UID:late-night-monday@test"
                                       , "DTSTAMP:20240101T000000Z"
                                       , "DTSTART;TZID=America/New_York:20240101T233000"
                                       , "DTEND;TZID=America/New_York:20240102T003000"
                                       , "RRULE:FREQ=WEEKLY;COUNT=2;BYDAY=MO"
                                       , "SUMMARY:Late Monday event"
                                       , "END:VEVENT"
                                       ]
                                )
                    in
                    case Parser.parse input of
                        Ok cal ->
                            Parser.expand
                                { start = Date.fromCalendarDate 2024 Time.Jan 1
                                , end = Date.fromCalendarDate 2024 Time.Jan 31
                                }
                                cal.events
                                |> List.map occurrenceStartMillis
                                |> Expect.equal
                                    [ toMillis "2024-01-02T04:30:00.000Z"
                                    , toMillis "2024-01-09T04:30:00.000Z"
                                    ]

                        Err err ->
                            Expect.fail err
            ]
        , describe "round-trip and API footguns"
            [ test "values constructed with the public recurrence builder should round-trip through the parser" <|
                \() ->
                    let
                        generated : String
                        generated =
                            Ical.generate
                                (Ical.config
                                    { id = "//tests//elm-ical//EN"
                                    , domain = "example.com"
                                    }
                                )
                                [ Ical.event
                                    { id = "invalid-bymonthday"
                                    , stamp = toIso8601 "2024-01-01T00:00:00.000Z"
                                    , time =
                                        Ical.withTime
                                            { start = toIso8601 "2024-01-01T09:00:00.000Z"
                                            , end = toIso8601 "2024-01-01T10:00:00.000Z"
                                            }
                                    , summary = "Invalid recurrence value"
                                    }
                                    |> Ical.withRecurrenceRule
                                        (Ical.rule (Recurrence.Monthly { every = 1 })
                                            |> Ical.withByMonthDay [ 0 ]
                                        )
                                ]
                    in
                    generated
                        |> Parser.parse
                        |> Result.map (\_ -> ())
                        |> Expect.equal (Ok ())
            ]
        ]


calendar : List String -> String
calendar lines =
    String.join "\u{000D}\n"
        ([ "BEGIN:VCALENDAR"
         , "VERSION:2.0"
         , "PRODID:-//test//EN"
         ]
            ++ lines
            ++ [ "END:VCALENDAR", "" ]
        )


easternTimeZone : List String
easternTimeZone =
    [ "BEGIN:VTIMEZONE"
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
    ]


occurrenceStartMillis : Parser.Occurrence -> Int
occurrenceStartMillis occurrence =
    case occurrence.time of
        Parser.WithTime { start } ->
            Time.posixToMillis start.posix

        Parser.AllDay { start } ->
            Time.posixToMillis (Time.millisToPosix ((Date.toRataDie start - 719163) * 86400000))

        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year start.month start.day
                |> Date.toRataDie
                |> (\rataDie -> (rataDie - 719163) * 86400000)
                |> (\millis ->
                        millis
                            + start.hour
                            * 3600000
                            + start.minute
                            * 60000
                            + start.second
                            * 1000
                   )


toIso8601 : String -> Time.Posix
toIso8601 string =
    case Iso8601.toTime string of
        Ok parsed ->
            parsed

        Err error ->
            Debug.todo (Debug.toString error)


toMillis : String -> Int
toMillis =
    toIso8601 >> Time.posixToMillis
