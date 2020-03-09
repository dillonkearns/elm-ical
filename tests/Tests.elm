module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time


eventGenerate details =
    """BEGIN:VEVENT
UID:uid1@example.com
DTSTAMP:19970714T170000Z
ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
"""
        ++ formatKeys keys
        ++ """
END:VEVENT"""


keys =
    [ ( "DTSTART", "20200404T000000Z" )
    , ( "DTEND", "20200404T050000Z" )
    , ( "SUMMARY", "Party" )
    ]


formatKeys nodes =
    nodes
        |> List.map
            (\( key, value ) ->
                String.concat
                    [ key
                    , ":"
                    , value
                    ]
            )
        |> String.join "\n"


suite : Test
suite =
    describe "ical event"
        [ test "single event" <|
            \() ->
                { start = Time.millisToPosix 1586044800

                -- Saturday, April 4, 2020 5:00:00 PM GMT-07:00
                , end = Time.millisToPosix 1586062800

                --stamp: new Date('Fr Oct 04 2013 23:34:53 UTC'),
                , created = "" -- new Date('Fr Oct 04 2013 23:34:53 UTC'),
                , lastModified = "" -- new Date('Fr Oct 04 2013 23:34:53 UTC'),
                , summary = "Party"
                }
                    |> eventGenerate
                    |> Expect.equal """BEGIN:VEVENT
UID:uid1@example.com
DTSTAMP:19970714T170000Z
ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
DTSTART:20200404T000000Z
DTEND:20200404T050000Z
SUMMARY:Party
END:VEVENT"""
        ]
