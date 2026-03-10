port module GenerateFixtures exposing (main)

import Date
import Ical
import Ical.Parser as Parser
import Json.Encode
import Time


port fixtures : Json.Encode.Value -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init = \() -> ( (), fixtures (encodeFixtures ()) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


encodeFixtures : () -> Json.Encode.Value
encodeFixtures () =
    Json.Encode.list encodeFixture
        [ ( "basic-feed", basicFeed )
        , ( "simple-event", simpleEvent )
        , ( "long-description", longDescription )
        , ( "all-day-event", allDayEvent )
        , ( "organizer", organizerEvent )
        , ( "double-quotes", doubleQuotes )
        , ( "emoji", emojiEvent )
        , ( "empty-fields", emptyFieldsEvent )
        , ( "special-email", specialEmailEvent )
        , ( "url-with-special-chars", urlWithSpecialChars )
        , ( "empty-calendar-fields", emptyCalendarFields )
        ]


encodeFixture : ( String, String ) -> Json.Encode.Value
encodeFixture ( name, ics ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "ics", Json.Encode.string ics )
        , ( "elmParsed", encodeElmParsed ics )
        ]


encodeElmParsed : String -> Json.Encode.Value
encodeElmParsed ics =
    case Parser.parse ics of
        Ok cal ->
            Json.Encode.object
                [ ( "ok", Json.Encode.bool True )
                , ( "specVersion", Json.Encode.string cal.specVersion )
                , ( "generatorProductId", Json.Encode.string cal.generatorProductId )
                , ( "events", Json.Encode.list encodeEvent cal.events )
                ]

        Err err ->
            Json.Encode.object
                [ ( "ok", Json.Encode.bool False )
                , ( "error", Json.Encode.string err )
                ]


encodeEvent : Parser.Event -> Json.Encode.Value
encodeEvent ev =
    Json.Encode.object
        (List.filterMap identity
            [ Just ( "uid", Json.Encode.string ev.uid )
            , Just ( "summary", encodeMaybe Json.Encode.string ev.summary )
            , Just ( "description", encodeMaybe Json.Encode.string ev.description )
            , Just ( "location", encodeMaybe Json.Encode.string ev.location )
            , Just ( "status", encodeMaybe encodeStatus ev.status )
            , Just ( "transp", encodeMaybe encodeTransparency ev.transparency )
            , ev.organizer
                |> Maybe.map
                    (\org ->
                        ( "organizer"
                        , Json.Encode.object
                            [ ( "email", Json.Encode.string org.email )
                            , ( "name", encodeMaybe Json.Encode.string org.name )
                            ]
                        )
                    )
            , Just ( "dtstamp", encodePosix ev.stamp )
            , ev.created |> Maybe.map (\posix -> ( "created", encodePosix posix ))
            , ev.lastModified |> Maybe.map (\posix -> ( "lastModified", encodePosix posix ))
            ]
            ++ encodeEventTime ev.time
        )


encodeEventTime : Parser.EventTime -> List ( String, Json.Encode.Value )
encodeEventTime eventTime =
    case eventTime of
        Parser.AllDay { start, end } ->
            ( "dtstart"
            , Json.Encode.object
                [ ( "type", Json.Encode.string "date" )
                , ( "year", Json.Encode.int (Date.year start) )
                , ( "month", Json.Encode.int (Date.monthNumber start) )
                , ( "day", Json.Encode.int (Date.day start) )
                ]
            )
                :: (case end of
                        Just endDate ->
                            [ ( "dtend"
                              , Json.Encode.object
                                    [ ( "type", Json.Encode.string "date" )
                                    , ( "year", Json.Encode.int (Date.year endDate) )
                                    , ( "month", Json.Encode.int (Date.monthNumber endDate) )
                                    , ( "day", Json.Encode.int (Date.day endDate) )
                                    ]
                              )
                            ]

                        Nothing ->
                            []
                   )

        Parser.WithTime { start, end } ->
            ( "dtstart", encodeResolvedTime start )
                :: (case end of
                        Just endTime ->
                            [ ( "dtend", encodeResolvedTime endTime ) ]

                        Nothing ->
                            []
                   )

        Parser.FloatingTime { start, end } ->
            ( "dtstart", encodeLocalDateTime start )
                :: (case end of
                        Just endTime ->
                            [ ( "dtend", encodeLocalDateTime endTime ) ]

                        Nothing ->
                            []
                   )


encodeResolvedTime : Parser.ResolvedTime -> Json.Encode.Value
encodeResolvedTime { posix, timeZoneName } =
    Json.Encode.object
        ([ ( "type", Json.Encode.string "datetime-utc" )
         , ( "year", Json.Encode.int (Time.toYear Time.utc posix) )
         , ( "month", Json.Encode.int (monthToInt (Time.toMonth Time.utc posix)) )
         , ( "day", Json.Encode.int (Time.toDay Time.utc posix) )
         , ( "hour", Json.Encode.int (Time.toHour Time.utc posix) )
         , ( "minute", Json.Encode.int (Time.toMinute Time.utc posix) )
         , ( "second", Json.Encode.int (Time.toSecond Time.utc posix) )
         ]
            ++ (case timeZoneName of
                    Just tz ->
                        [ ( "timeZoneName", Json.Encode.string tz ) ]

                    Nothing ->
                        []
               )
        )


encodeLocalDateTime : Parser.LocalDateTime -> Json.Encode.Value
encodeLocalDateTime { year, month, day, hour, minute, second } =
    Json.Encode.object
        [ ( "type", Json.Encode.string "datetime-local" )
        , ( "year", Json.Encode.int year )
        , ( "month", Json.Encode.int (monthToInt month) )
        , ( "day", Json.Encode.int day )
        , ( "hour", Json.Encode.int hour )
        , ( "minute", Json.Encode.int minute )
        , ( "second", Json.Encode.int second )
        ]


encodePosix : Time.Posix -> Json.Encode.Value
encodePosix posix =
    Json.Encode.object
        [ ( "type", Json.Encode.string "datetime-utc" )
        , ( "year", Json.Encode.int (Time.toYear Time.utc posix) )
        , ( "month", Json.Encode.int (monthToInt (Time.toMonth Time.utc posix)) )
        , ( "day", Json.Encode.int (Time.toDay Time.utc posix) )
        , ( "hour", Json.Encode.int (Time.toHour Time.utc posix) )
        , ( "minute", Json.Encode.int (Time.toMinute Time.utc posix) )
        , ( "second", Json.Encode.int (Time.toSecond Time.utc posix) )
        ]


encodeStatus : Parser.Status -> Json.Encode.Value
encodeStatus status =
    Json.Encode.string
        (case status of
            Parser.Tentative ->
                "TENTATIVE"

            Parser.Confirmed ->
                "CONFIRMED"

            Parser.Cancelled ->
                "CANCELLED"
        )


encodeTransparency : Parser.Transparency -> Json.Encode.Value
encodeTransparency transparency =
    Json.Encode.string
        (case transparency of
            Parser.Opaque ->
                "OPAQUE"

            Parser.Transparent ->
                "TRANSPARENT"
        )


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe encoder maybe =
    case maybe of
        Just val ->
            encoder val

        Nothing ->
            Json.Encode.null


basicFeed : String
basicFeed =
    [ Ical.event
        { id = "1"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1381100100000
                }
        , summary = "repeating by month"
        }
        |> Ical.withDescription "repeating by month"
    , Ical.event
        { id = "2"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1381100100000
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


simpleEvent : String
simpleEvent =
    [ Ical.event
        { id = "123"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Simple Event"
        }
        |> Ical.withCreated (millisToPosix 1380929693000)
        |> Ical.withLastModified (millisToPosix 1380929693000)
    ]
        |> Ical.generate
            (Ical.config
                { id = "//sebbo.net//ical-generator.tests//EN"
                , domain = "sebbo.net"
                }
            )


longDescription : String
longDescription =
    [ Ical.event
        { id = "123"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
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


allDayEvent : String
allDayEvent =
    [ Ical.event
        { id = "4ot852po37bvri1natdlv4cf6r"
        , stamp = millisToPosix 1616083244000
        , time =
            Ical.allDay (Date.fromCalendarDate 2021 Time.Mar 18)
        , summary = "All day event"
        }
        |> Ical.withCreated (millisToPosix 1616079577000)
        |> Ical.withLastModified (millisToPosix 1616079577000)
        |> Ical.withStatus Ical.Confirmed
        |> Ical.withTransparency Ical.Transparent
    ]
        |> Ical.generate
            (Ical.config
                { id = "//sebbo.net//ical-generator.tests//EN"
                , domain = "incrementalelm.com"
                }
            )


organizerEvent : String
organizerEvent =
    [ Ical.event
        { id = "org-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Event with organizer"
        }
        |> Ical.withOrganizer
            { name = "Dillon Kearns"
            , email = "dillon@incrementalelm.com"
            }
    ]
        |> Ical.generate
            (Ical.config
                { id = "//test//test//EN"
                , domain = "test.com"
                }
            )


doubleQuotes : String
doubleQuotes =
    [ Ical.event
        { id = "quote-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
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


emojiEvent : String
emojiEvent =
    [ Ical.event
        { id = "emoji-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = String.repeat 60 "a" ++ "🎉🎊🥳" ++ "bbb"
        }
    ]
        |> Ical.generate
            (Ical.config
                { id = "//test//test//EN"
                , domain = "test.com"
                }
            )


emptyFieldsEvent : String
emptyFieldsEvent =
    [ Ical.event
        { id = "empty-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Event with empty fields"
        }
        |> Ical.withDescription ""
        |> Ical.withLocation ""
    ]
        |> Ical.generate
            (Ical.config
                { id = "//test//test//EN"
                , domain = "test.com"
                }
            )


specialEmailEvent : String
specialEmailEvent =
    [ Ical.event
        { id = "special-email-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Event with special email"
        }
        |> Ical.withOrganizer
            { name = "Test User"
            , email = "user;tag@example.com"
            }
    ]
        |> Ical.generate
            (Ical.config
                { id = "//test//test//EN"
                , domain = "test.com"
                }
            )


urlWithSpecialChars : String
urlWithSpecialChars =
    [ Ical.event
        { id = "url-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Event with URL"
        }
    ]
        |> Ical.generate
            (Ical.config
                { id = "//test//test//EN"
                , domain = "test.com"
                }
                |> Ical.withUrl "https://example.com/cal?a=1,2;b=3"
            )


emptyCalendarFields : String
emptyCalendarFields =
    [ Ical.event
        { id = "empty-cal-test"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.withTime
                { start = millisToPosix 1380926370000
                , end = millisToPosix 1380928500000
                }
        , summary = "Event with empty cal fields"
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


millisToPosix : Int -> Time.Posix
millisToPosix =
    Time.millisToPosix
