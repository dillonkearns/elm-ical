module Ical exposing
    ( Config, config, withName, withCalendarDescription, withUrl
    , Event, event, EventTime(..)
    , withDescription, withLocation, withOrganizer, withHtmlDescription
    , withStatus, Status(..), withTransparency, EventTransparency(..)
    , withCreated, withLastModified
    , Recipient
    , generate, generateEvent
    )

{-| Generate iCal (RFC 5545) calendar feeds.

@docs Config, config, withName, withCalendarDescription, withUrl
@docs Event, event, EventTime
@docs withDescription, withLocation, withOrganizer, withHtmlDescription
@docs withStatus, Status, withTransparency, EventTransparency
@docs withCreated, withLastModified
@docs Recipient
@docs generate, generateEvent

-}

import Date exposing (Date)
import Property exposing (Parameter(..), ValueData(..))
import Time


{-| Represents the time span of an event. Either all-day (date only) or with
specific times.

For `AllDay`, the `end` date is **inclusive** — a single-day event on March 18
should use `AllDay { start = march18, end = march18 }`. The library
automatically adds one day to produce the exclusive DTEND required by iCal.

-}
type EventTime
    = AllDay { start : Date, end : Date }
    | WithTime { start : Time.Posix, end : Time.Posix }


{-| An opaque type representing a calendar event. Create one with [`event`](#event)
and customize it with the `with*` functions.
-}
type Event
    = Event EventData


type alias EventData =
    { id : String
    , stamp : Time.Posix
    , time : EventTime
    , summary : String
    , description : Maybe String
    , location : Maybe String
    , organizer : Maybe Recipient
    , htmlDescription : Maybe String
    , transparency : Maybe EventTransparency
    , status : Maybe Status
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
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


{-| A person with a name and email address, used for the ORGANIZER property.
-}
type alias Recipient =
    { name : String
    , email : String
    }


{-| An opaque type representing calendar configuration. Create one with
[`config`](#config) and customize with [`withName`](#withName),
[`withCalendarDescription`](#withCalendarDescription), and [`withUrl`](#withUrl).
-}
type Config
    = Config ConfigData


type alias ConfigData =
    { id : String
    , domain : String
    , name : Maybe String
    , description : Maybe String
    , url : Maybe String
    }



-- Config builder


{-| Create a calendar configuration with the required fields.

    Ical.config { id = "//myapp//calendar//EN", domain = "example.com" }

-}
config : { id : String, domain : String } -> Config
config { id, domain } =
    Config
        { id = id
        , domain = domain
        , name = Nothing
        , description = Nothing
        , url = Nothing
        }


{-| Set the calendar display name (NAME property).
-}
withName : String -> Config -> Config
withName name (Config c) =
    Config { c | name = Just name }


{-| Set the calendar description (DESCRIPTION property).
-}
withCalendarDescription : String -> Config -> Config
withCalendarDescription description (Config c) =
    Config { c | description = Just description }


{-| Set the calendar URL.
-}
withUrl : String -> Config -> Config
withUrl url (Config c) =
    Config { c | url = Just url }



-- Event builder


{-| Create an event with the required fields.

    Ical.event
        { id = "unique-id-123"
        , stamp = timestamp
        , time = Ical.WithTime { start = startTime, end = endTime }
        , summary = "Team Meeting"
        }

-}
event : { id : String, stamp : Time.Posix, time : EventTime, summary : String } -> Event
event { id, stamp, time, summary } =
    Event
        { id = id
        , stamp = stamp
        , time = time
        , summary = summary
        , description = Nothing
        , location = Nothing
        , organizer = Nothing
        , htmlDescription = Nothing
        , transparency = Nothing
        , status = Nothing
        , created = Nothing
        , lastModified = Nothing
        }


{-| Set the event description (DESCRIPTION property).
-}
withDescription : String -> Event -> Event
withDescription description (Event e) =
    Event { e | description = Just description }


{-| Set the event location (LOCATION property).
-}
withLocation : String -> Event -> Event
withLocation location (Event e) =
    Event { e | location = Just location }


{-| Set the event organizer (ORGANIZER property with CN parameter).
-}
withOrganizer : Recipient -> Event -> Event
withOrganizer organizer (Event e) =
    Event { e | organizer = Just organizer }


{-| Set the HTML description (X-ALT-DESC property with FMTTYPE=text/html).
-}
withHtmlDescription : String -> Event -> Event
withHtmlDescription html (Event e) =
    Event { e | htmlDescription = Just html }


{-| Set the event status.
-}
withStatus : Status -> Event -> Event
withStatus status (Event e) =
    Event { e | status = Just status }


{-| Set the event transparency.
-}
withTransparency : EventTransparency -> Event -> Event
withTransparency transparency (Event e) =
    Event { e | transparency = Just transparency }


{-| Set the CREATED timestamp.
-}
withCreated : Time.Posix -> Event -> Event
withCreated created (Event e) =
    Event { e | created = Just created }


{-| Set the LAST-MODIFIED timestamp.
-}
withLastModified : Time.Posix -> Event -> Event
withLastModified lastModified (Event e) =
    Event { e | lastModified = Just lastModified }



-- Generating output


{-| Generate a complete iCal calendar string with the given config and events.
-}
generate : Config -> List Event -> String
generate ((Config c) as cfg) events =
    "BEGIN:VCALENDAR\u{000D}\n"
        ++ calendarProperties c
        ++ "\u{000D}\n"
        ++ String.join "\u{000D}\n" (List.map (generateEvent cfg) events)
        ++ "\u{000D}\nEND:VCALENDAR\u{000D}\n"


{-| Generate the iCal string for a single VEVENT.
-}
generateEvent : Config -> Event -> String
generateEvent (Config c) (Event details) =
    "BEGIN:VEVENT\u{000D}\n"
        ++ formatProperties (eventProperties c details)
        ++ "\u{000D}\nEND:VEVENT"


eventProperties : ConfigData -> EventData -> List ( String, ValueData, List Parameter )
eventProperties c details =
    timeProperties details.time
        ++ [ ( "DTSTAMP", details.stamp |> Property.DateTime, [] )
           , ( "UID", details.id ++ "@" ++ c.domain |> Text, [] )
           , ( "SUMMARY", details.summary |> Text, [] )
           ]
        ++ ([ details.created |> Maybe.map (\created -> ( "CREATED", created |> Property.DateTime, [] ))
            , details.lastModified |> Maybe.map (\lastModified -> ( "LAST-MODIFIED", lastModified |> Property.DateTime, [] ))
            , details.location |> Maybe.andThen nonEmpty |> Maybe.map (\location -> ( "LOCATION", location |> Text, [] ))
            , details.description |> Maybe.andThen nonEmpty |> Maybe.map (\description -> ( "DESCRIPTION", Text description, [] ))
            , details.htmlDescription |> Maybe.andThen nonEmpty |> Maybe.map (\htmlDescription -> ( "X-ALT-DESC", htmlDescription |> Text, [ Parameter ( "FMTTYPE", "text/html" ) ] ))
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


timeProperties : EventTime -> List ( String, ValueData, List Parameter )
timeProperties eventTime =
    let
        dateParam : List Parameter
        dateParam =
            [ Parameter ( "VALUE", "DATE" ) ]
    in
    case eventTime of
        AllDay { start, end } ->
            [ ( "DTSTART", Property.DateValue start, dateParam )
            , ( "DTEND", Property.DateValue (Date.add Date.Days 1 end), dateParam )
            ]

        WithTime { start, end } ->
            [ ( "DTSTART", Property.DateTime start, [] )
            , ( "DTEND", Property.DateTime end, [] )
            ]


statusToString : Status -> String
statusToString status =
    case status of
        Tentative ->
            "TENTATIVE"

        Confirmed ->
            "CONFIRMED"

        Cancelled ->
            "CANCELLED"


transparencyToString : EventTransparency -> String
transparencyToString transparency =
    case transparency of
        Transparent ->
            "TRANSPARENT"

        Opaque ->
            "OPAQUE"


nonEmpty : String -> Maybe String
nonEmpty s =
    if String.isEmpty s then
        Nothing

    else
        Just s


formatProperties : List ( String, ValueData, List Parameter ) -> String
formatProperties nodes =
    nodes
        |> List.map Property.encodeProperty
        |> String.join "\u{000D}\n"


calendarProperties : ConfigData -> String
calendarProperties c =
    [ ( "VERSION", "2.0" |> Text, [] )
    , ( "PRODID", "-" ++ c.id |> Text, [] )
    ]
        ++ ([ c.name |> Maybe.andThen nonEmpty |> Maybe.map (\name -> ( "NAME", Text name, [] ))
            , c.description |> Maybe.andThen nonEmpty |> Maybe.map (\description -> ( "DESCRIPTION", Text description, [] ))
            , c.url |> Maybe.andThen nonEmpty |> Maybe.map (\url -> ( "URL", Property.Uri url, [] ))
            ]
                |> List.filterMap identity
           )
        |> formatProperties
