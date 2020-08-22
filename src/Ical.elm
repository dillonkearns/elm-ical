module Ical exposing (generate)

import Format
import Iso8601
import Rfc3339
import Time


type alias Event =
    { stamp : Time.Posix
    , start : Time.Posix
    , end : Time.Posix
    , summary : String
    , id : String
    }


eventGenerate : Config -> Event -> String
eventGenerate config details =
    """BEGIN:VEVENT
"""
        ++ formatKeys (keys config details)
        ++ """
END:VEVENT"""


keys : Config -> Event -> List ( String, String )
keys config details =
    [ ( "UID", details.id ++ "@" ++ config.domain )
    , ( "DTSTAMP", details.stamp |> Rfc3339.format ) -- https://www.kanzaki.com/docs/ical/dtstamp.html
    , ( "DTSTART", details.start |> Rfc3339.format )
    , ( "DTEND", details.end |> Rfc3339.format )
    , ( "SUMMARY", details.summary )
    ]


formatKeys : List ( String, String ) -> String
formatKeys nodes =
    nodes
        |> List.map Format.normalizeField
        |> String.join "\n"


type alias Config =
    { id : String, domain : String }


generate : Config -> List Event -> String
generate config events =
    """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//incrementalelm.com//elm-ical.tests//EN
""" ++ String.join "\n" (List.map (eventGenerate config) events) ++ """
END:VCALENDAR"""
