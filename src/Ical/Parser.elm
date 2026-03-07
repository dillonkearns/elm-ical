module Ical.Parser exposing
    ( parse
    , Calendar, Event
    , DateTimeValue(..), TimeZone(..)
    , Organizer, RawProperty
    )

{-| Parse iCal (RFC 5545) calendar strings.

@docs parse
@docs Calendar, Event
@docs DateTimeValue, TimeZone
@docs Organizer, RawProperty

-}

import ContentLine exposing (ContentLine)
import ValueParser


{-| A parsed iCal calendar.
-}
type alias Calendar =
    { prodId : Maybe String
    , version : Maybe String
    , events : List Event
    , properties : List RawProperty
    }


{-| A parsed iCal event. All fields are `Maybe` since parsed input may
not contain every property.
-}
type alias Event =
    { uid : Maybe String
    , dtstamp : Maybe DateTimeValue
    , dtstart : Maybe DateTimeValue
    , dtend : Maybe DateTimeValue
    , created : Maybe DateTimeValue
    , lastModified : Maybe DateTimeValue
    , summary : Maybe String
    , description : Maybe String
    , location : Maybe String
    , organizer : Maybe Organizer
    , status : Maybe String
    , transp : Maybe String
    , properties : List RawProperty
    }


{-| A parsed date or date-time value.

  - `DateOnly` — an all-day value with just year, month, day (iCal `VALUE=DATE`)
  - `DateTime` — a date plus time-of-day with a [`TimeZone`](#TimeZone) context

-}
type DateTimeValue
    = DateOnly { year : Int, month : Int, day : Int }
    | DateTime { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int } TimeZone


{-| The timezone context of a `DateTime` value.

  - `Utc` — the value ends with `Z` (e.g. `20210318T162044Z`)
  - `Floating` — no timezone indicator; interpreted in the viewer's local time
  - `Tzid` — an explicit IANA timezone (e.g. `America/New_York`)

-}
type TimeZone
    = Utc
    | Floating
    | Tzid String


{-| A parsed ORGANIZER property.
-}
type alias Organizer =
    { calAddress : String
    , commonName : Maybe String
    }


{-| An uninterpreted property — name, parameters, and raw value string.
Unknown properties and extension properties (`X-*`) are preserved here.
-}
type alias RawProperty =
    { name : String
    , parameters : List ( String, String )
    , value : String
    }


{-| Parse an iCal string into a Calendar.

    Ical.Parser.parse icsString
    --> Ok { prodId = Just "-//My App//EN", version = Just "2.0", events = [...], ... }

-}
parse : String -> Result String Calendar
parse input =
    let
        unfolded : String
        unfolded =
            ContentLine.unfold input

        lines : List String
        lines =
            splitLines unfolded
                |> List.filter (not << String.isEmpty)
    in
    case lines of
        [] ->
            Err "Empty input"

        _ ->
            parseLines lines


splitLines : String -> List String
splitLines input =
    input
        |> String.replace "\u{000D}\n" "\n"
        |> String.split "\n"


parseLines : List String -> Result String Calendar
parseLines lines =
    let
        contentLines : List ContentLine
        contentLines =
            List.filterMap
                (\line ->
                    case ContentLine.parse line of
                        Ok cl ->
                            Just cl

                        Err _ ->
                            Nothing
                )
                lines
    in
    parseCalendar contentLines


parseCalendar : List ContentLine -> Result String Calendar
parseCalendar contentLines =
    case contentLines of
        first :: rest ->
            if first.name == "BEGIN" && String.toUpper first.value == "VCALENDAR" then
                parseCalendarBody rest emptyCalendar

            else
                Err "Expected BEGIN:VCALENDAR"

        [] ->
            Err "Empty input"


emptyCalendar : Calendar
emptyCalendar =
    { prodId = Nothing
    , version = Nothing
    , events = []
    , properties = []
    }


parseCalendarBody : List ContentLine -> Calendar -> Result String Calendar
parseCalendarBody lines cal =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VCALENDAR"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VCALENDAR" then
                Ok { cal | events = List.reverse cal.events, properties = List.reverse cal.properties }

            else if line.name == "END" then
                Err ("Mismatched END: expected VCALENDAR, got " ++ line.value)

            else if line.name == "BEGIN" && String.toUpper line.value == "VEVENT" then
                case parseEvent rest emptyEvent of
                    Ok ( event, remaining ) ->
                        parseCalendarBody remaining { cal | events = event :: cal.events }

                    Err err ->
                        Err err

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        parseCalendarBody remaining cal

                    Err err ->
                        Err err

            else
                let
                    updatedCal : Calendar
                    updatedCal =
                        case line.name of
                            "VERSION" ->
                                { cal | version = Just line.value }

                            "PRODID" ->
                                { cal | prodId = Just line.value }

                            _ ->
                                { cal
                                    | properties =
                                        { name = line.name
                                        , parameters = line.parameters
                                        , value = line.value
                                        }
                                            :: cal.properties
                                }
                in
                parseCalendarBody rest updatedCal


emptyEvent : Event
emptyEvent =
    { uid = Nothing
    , dtstamp = Nothing
    , dtstart = Nothing
    , dtend = Nothing
    , created = Nothing
    , lastModified = Nothing
    , summary = Nothing
    , description = Nothing
    , location = Nothing
    , organizer = Nothing
    , status = Nothing
    , transp = Nothing
    , properties = []
    }


parseEvent : List ContentLine -> Event -> Result String ( Event, List ContentLine )
parseEvent lines ev =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VEVENT"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VEVENT" then
                Ok ( { ev | properties = List.reverse ev.properties }, rest )

            else if line.name == "END" then
                Err ("Mismatched END: expected VEVENT, got " ++ line.value)

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        parseEvent remaining ev

                    Err err ->
                        Err err

            else
                parseEvent rest (applyEventProperty line ev)


applyEventProperty : ContentLine -> Event -> Event
applyEventProperty line ev =
    case line.name of
        "UID" ->
            { ev | uid = Just line.value }

        "DTSTAMP" ->
            { ev | dtstamp = parseDateTimeValue line |> Result.toMaybe }

        "DTSTART" ->
            { ev | dtstart = parseDateTimeValue line |> Result.toMaybe }

        "DTEND" ->
            { ev | dtend = parseDateTimeValue line |> Result.toMaybe }

        "CREATED" ->
            { ev | created = parseDateTimeValue line |> Result.toMaybe }

        "LAST-MODIFIED" ->
            { ev | lastModified = parseDateTimeValue line |> Result.toMaybe }

        "SUMMARY" ->
            { ev | summary = Just (ValueParser.unescapeText line.value) }

        "DESCRIPTION" ->
            { ev | description = Just (ValueParser.unescapeText line.value) }

        "LOCATION" ->
            { ev | location = Just (ValueParser.unescapeText line.value) }

        "ORGANIZER" ->
            { ev | organizer = Just (parseOrganizer line) }

        "STATUS" ->
            { ev | status = Just line.value }

        "TRANSP" ->
            { ev | transp = Just line.value }

        _ ->
            { ev
                | properties =
                    { name = line.name
                    , parameters = line.parameters
                    , value = line.value
                    }
                        :: ev.properties
            }


parseDateTimeValue : ContentLine -> Result String DateTimeValue
parseDateTimeValue line =
    let
        isDate : Bool
        isDate =
            line.parameters
                |> List.any (\( k, v ) -> String.toUpper k == "VALUE" && String.toUpper v == "DATE")

        tzid : Maybe String
        tzid =
            line.parameters
                |> List.filterMap
                    (\( k, v ) ->
                        if String.toUpper k == "TZID" then
                            Just v

                        else
                            Nothing
                    )
                |> List.head
    in
    if isDate then
        ValueParser.parseDate line.value
            |> Result.map DateOnly

    else
        ValueParser.parseDateTime line.value
            |> Result.map (dateTimePartsToValue tzid)


dateTimePartsToValue : Maybe String -> ValueParser.DateTimeParts -> DateTimeValue
dateTimePartsToValue maybeTzid parts =
    let
        dt : { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
        dt =
            { year = parts.year
            , month = parts.month
            , day = parts.day
            , hour = parts.hour
            , minute = parts.minute
            , second = parts.second
            }

        timezone : TimeZone
        timezone =
            case maybeTzid of
                Just tz ->
                    Tzid tz

                Nothing ->
                    if parts.isUtc then
                        Utc

                    else
                        Floating
    in
    DateTime dt timezone


parseOrganizer : ContentLine -> Organizer
parseOrganizer line =
    let
        cn : Maybe String
        cn =
            line.parameters
                |> List.filterMap
                    (\( k, v ) ->
                        if String.toUpper k == "CN" then
                            Just v

                        else
                            Nothing
                    )
                |> List.head
    in
    { calAddress = line.value
    , commonName = cn
    }


skipComponent : String -> List ContentLine -> Result String (List ContentLine)
skipComponent componentName lines =
    case lines of
        [] ->
            Err ("Unexpected end of input, expected END:" ++ componentName)

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == componentName then
                Ok rest

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        skipComponent componentName remaining

                    Err err ->
                        Err err

            else
                skipComponent componentName rest
