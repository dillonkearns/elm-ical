module Ical exposing (Config, Event, EventTransparency(..), Recipient, Status(..), eventGenerate, generate)

import Property exposing (Parameter(..), ValueData(..))
import Time


type alias Event =
    { stamp : Time.Posix
    , start : Property.DateOrDateTime
    , end : Property.DateOrDateTime
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , summary : String
    , description : Maybe String
    , id : String
    , organizer : Maybe Recipient
    , location : Maybe String
    , htmlDescription : Maybe String
    , transparency : Maybe EventTransparency
    , status : Maybe Status
    }


{-| <https://tools.ietf.org/html/rfc5545#section-3.8.2.7>

"OPAQUE" - Blocks or opaque on busy time searches.
"TRANSPARENT" - Transparent on busy time searches.
Default value is OPAQUE.

-}
type EventTransparency
    = Opaque
    | Transparent


{-| <https://tools.ietf.org/html/rfc5545#section-3.8.1.11>

       statvalue-event = "TENTATIVE"    ;Indicates event is tentative.
                       / "CONFIRMED"    ;Indicates event is definite.
                       / "CANCELLED"    ;Indicates event was cancelled.

-}
type Status
    = Tentative
    | Confirmed
    | Cancelled


statusToString : Status -> String
statusToString status =
    case status of
        Tentative ->
            "TENTATIVE"

        Confirmed ->
            "CONFIRMED"

        Cancelled ->
            "CANCELLED"



--OPAQUE | TRANSPARENT


type alias Recipient =
    { name : String
    , email : String

    --, mailTo : Maybe String
    }


eventGenerate : Config -> Event -> String
eventGenerate config details =
    """BEGIN:VEVENT
"""
        ++ formatKeysNew (keysNew config details)
        ++ """
END:VEVENT"""


paramForDateOrTime : Property.DateOrDateTime -> List Parameter
paramForDateOrTime dateOrTime =
    case dateOrTime of
        Property.Date _ ->
            [ Parameter ( "VALUE", "DATE" ) ]

        Property.DateWithTime _ ->
            []


keysNew : Config -> Event -> List ( String, ValueData, List Parameter )
keysNew config details =
    [ ( "DTSTART"
      , details.start |> Property.DateOrTime
      , paramForDateOrTime details.start
      )

    --[ ( "DTSTART", Property.Text "20210318", [ Parameter ( "VALUE", "DATE" ) ] )
    --, ( "DTEND", Property.Text "20210319", [ Parameter ( "VALUE", "DATE" ) ] )
    , ( "DTEND"
      , details.end |> Property.DateOrTime
      , paramForDateOrTime details.start
      )
    , ( "DTSTAMP", details.stamp |> Property.DateTime, [] ) -- https://www.kanzaki.com/docs/ical/dtstamp.html
    , ( "UID", details.id ++ "@" ++ config.domain |> Text, [] )
    , ( "SUMMARY", details.summary |> Text, [] )
    ]
        ++ ([ details.created |> Maybe.map (\created -> ( "CREATED", created |> Property.DateTime, [] ))
            , details.lastModified |> Maybe.map (\lastModified -> ( "LAST-MODIFIED", lastModified |> Property.DateTime, [] ))
            , details.location |> Maybe.map (\location -> ( "LOCATION", location |> Text, [] ))
            , details.description |> Maybe.map (\description -> ( "DESCRIPTION", Text description, [] ))
            , details.htmlDescription |> Maybe.map (\htmlDescription -> ( "X-ALT-DESC;FMTTYPE=text/html", htmlDescription |> Text, [] ))
            , details.status |> Maybe.map (\status -> ( "STATUS", status |> statusToString |> Text, [] ))
            , details.transparency |> Maybe.map (\transparency -> ( "TRANSP", transparency |> transparencyToString |> Text, [] ))
            , details.organizer
                |> Maybe.map
                    (\organizer ->
                        ( "ORGANIZER"
                        , CalAddress organizer.email
                        , [ Parameter ( "CN", organizer.name ) ]
                        )
                    )
            ]
                |> List.filterMap identity
           )


transparencyToString : EventTransparency -> String
transparencyToString transparency =
    case transparency of
        Transparent ->
            "TRANSPARENT"

        Opaque ->
            "OPAQUE"


formatKeysNew : List ( String, ValueData, List Parameter ) -> String
formatKeysNew nodes =
    nodes
        |> List.map Property.encodeProperty
        |> String.join "\n"


type alias Config =
    { id : String
    , domain : String
    , name : Maybe String
    , description : Maybe String
    , url : Maybe String
    }


generate : Config -> List Event -> String
generate config events =
    """BEGIN:VCALENDAR
"""
        ++ calendarProperties config
        ++ "\n"
        ++ String.join "\n" (List.map (eventGenerate config) events)
        ++ """
END:VCALENDAR"""


calendarProperties : Config -> String
calendarProperties config =
    [ ( "VERSION", "2.0" |> Text, [] )
    , ( "PRODID", "-" ++ config.id |> Text, [] )
    ]
        ++ ([ config.name |> Maybe.map (\name -> ( "NAME", Text name, [] ))
            , config.description |> Maybe.map (\description -> ( "DESCRIPTION", Text description, [] ))
            , config.url |> Maybe.map (\url -> ( "URL", Text url, [] ))
            ]
                |> List.filterMap identity
           )
        |> formatKeysNew
