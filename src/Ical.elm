module Ical exposing
    ( Config, config, withName, withCalendarDescription, withUrl
    , Event, event, EventTime(..), Organizer
    , withDescription, withLocation, withOrganizer, withHtmlDescription
    , withStatus, Status(..), withTransparency, Transparency(..)
    , withCreated, withLastModified
    , withRecurrenceRule, withAttendee
    , generate, generateEvent
    )

{-| Generate iCal ([RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545)) calendar feeds.


## Calendar configuration

@docs Config, config, withName, withCalendarDescription, withUrl


## Events

@docs Event, event, EventTime, Organizer
@docs withDescription, withLocation, withOrganizer, withHtmlDescription
@docs withStatus, Status, withTransparency, Transparency
@docs withCreated, withLastModified


## Generating output

@docs withRecurrenceRule, withAttendee
@docs generate, generateEvent

-}

import Date exposing (Date)
import Ical.Recurrence as Recurrence exposing (RecurrenceRule)
import IcalDateTime
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
    , organizer : Maybe Organizer
    , htmlDescription : Maybe String
    , transparency : Maybe Transparency
    , status : Maybe Status
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , recurrenceRule : Maybe RecurrenceRule
    , attendees : List Organizer
    }


{-| <https://tools.ietf.org/html/rfc5545#section-3.8.2.7>

"OPAQUE" - Blocks or opaque on busy time searches.
"TRANSPARENT" - Transparent on busy time searches.
Default value is OPAQUE.

-}
type Transparency
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
type alias Organizer =
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
        , recurrenceRule = Nothing
        , attendees = []
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
withOrganizer : Organizer -> Event -> Event
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
withTransparency : Transparency -> Event -> Event
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


{-| Add a recurrence rule (RRULE) to the event. See [`Ical.Recurrence`](Ical-Recurrence)
for the `RecurrenceRule` type.
-}
withRecurrenceRule : RecurrenceRule -> Event -> Event
withRecurrenceRule rule (Event e) =
    Event { e | recurrenceRule = Just rule }


{-| Add an attendee to the event (ATTENDEE property with CN parameter).
-}
withAttendee : { email : String, name : String } -> Event -> Event
withAttendee attendee (Event e) =
    Event { e | attendees = e.attendees ++ [ { name = attendee.name, email = attendee.email } ] }



-- Generating output


{-| Generate a complete iCal calendar string with the given config and events.
-}
generate : Config -> List Event -> String
generate ((Config c) as cfg) events =
    let
        eventSection : String
        eventSection =
            case List.map (generateEvent cfg) events of
                [] ->
                    ""

                generatedEvents ->
                    "\u{000D}\n" ++ String.join "\u{000D}\n" generatedEvents
    in
    "BEGIN:VCALENDAR\u{000D}\n"
        ++ calendarProperties c
        ++ eventSection
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
            , details.recurrenceRule
                |> Maybe.map
                    (\rule ->
                        ( "RRULE"
                        , Uri (formatRecurrenceRule rule)
                        , []
                        )
                    )
            ]
                |> List.filterMap identity
           )
        ++ List.map
            (\attendee ->
                ( "ATTENDEE"
                , CalAddress attendee.email
                , [ Parameter ( "CN", attendee.name ) ]
                )
            )
            details.attendees


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


transparencyToString : Transparency -> String
transparencyToString transparency =
    case transparency of
        Transparent ->
            "TRANSPARENT"

        Opaque ->
            "OPAQUE"


formatRecurrenceRule : RecurrenceRule -> String
formatRecurrenceRule rule =
    let
        freq : String
        freq =
            "FREQ="
                ++ (case rule.frequency of
                        Recurrence.Daily ->
                            "DAILY"

                        Recurrence.Weekly ->
                            "WEEKLY"

                        Recurrence.Monthly ->
                            "MONTHLY"

                        Recurrence.Yearly ->
                            "YEARLY"
                   )

        parts : List (Maybe String)
        parts =
            [ Just freq
            , if rule.interval > 1 then
                Just ("INTERVAL=" ++ String.fromInt rule.interval)

              else
                Nothing
            , case rule.end of
                Recurrence.Forever ->
                    Nothing

                Recurrence.Count n ->
                    Just ("COUNT=" ++ String.fromInt n)

                Recurrence.UntilDate date ->
                    Just ("UNTIL=" ++ IcalDateTime.formatDate date)

                Recurrence.UntilDateTime posix ->
                    Just ("UNTIL=" ++ IcalDateTime.format posix)
            , if List.isEmpty rule.byDay then
                Nothing

              else
                Just ("BYDAY=" ++ String.join "," (List.map formatDaySpec rule.byDay))
            , if List.isEmpty rule.byMonthDay then
                Nothing

              else
                Just ("BYMONTHDAY=" ++ String.join "," (List.map String.fromInt rule.byMonthDay))
            , if List.isEmpty rule.byMonth then
                Nothing

              else
                Just ("BYMONTH=" ++ String.join "," (List.map String.fromInt rule.byMonth))
            , if List.isEmpty rule.bySetPos then
                Nothing

              else
                Just ("BYSETPOS=" ++ String.join "," (List.map String.fromInt rule.bySetPos))
            , if rule.weekStart /= Time.Mon then
                Just ("WKST=" ++ weekdayToString rule.weekStart)

              else
                Nothing
            ]
    in
    parts
        |> List.filterMap identity
        |> String.join ";"


formatDaySpec : Recurrence.DaySpec -> String
formatDaySpec spec =
    let
        prefix : String
        prefix =
            case spec.ordinal of
                Just n ->
                    String.fromInt n

                Nothing ->
                    ""
    in
    prefix ++ weekdayToString spec.weekday


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "MO"

        Time.Tue ->
            "TU"

        Time.Wed ->
            "WE"

        Time.Thu ->
            "TH"

        Time.Fri ->
            "FR"

        Time.Sat ->
            "SA"

        Time.Sun ->
            "SU"


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
