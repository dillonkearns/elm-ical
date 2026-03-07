port module GenerateFixtures exposing (main)

import Date
import Ical
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
        ]


encodeFixture : ( String, String ) -> Json.Encode.Value
encodeFixture ( name, ics ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "ics", Json.Encode.string ics )
        ]


basicFeed : String
basicFeed =
    [ Ical.event
        { id = "1"
        , stamp = millisToPosix 1380929693000
        , time =
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.AllDay
                { start = Date.fromCalendarDate 2021 Time.Mar 18
                , end = Date.fromCalendarDate 2021 Time.Mar 18
                }
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
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.WithTime
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
            Ical.WithTime
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


millisToPosix : Int -> Time.Posix
millisToPosix =
    Time.millisToPosix
