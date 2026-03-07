module Ical.Parse exposing (parse, Calendar, Event, DateTimeValue(..), Organizer, RawProperty)

{-| Parse iCal (RFC 5545) calendar strings.

@docs parse, Calendar, Event, DateTimeValue, Organizer, RawProperty

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


{-| A parsed iCal event.
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
-}
type DateTimeValue
    = DateOnly { year : Int, month : Int, day : Int }
    | DateTimeUtc { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
    | DateTimeLocal { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
    | DateTimeWithTzid { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int, tzid : String }


{-| A parsed organizer.
-}
type alias Organizer =
    { calAddress : String
    , commonName : Maybe String
    , parameters : List ( String, String )
    }


{-| A raw property.
-}
type alias RawProperty =
    { name : String
    , parameters : List ( String, String )
    , value : String
    }


{-| Parse an iCal string into a Calendar.
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
        case tzid of
            Just tz ->
                ValueParser.parseDateTime line.value
                    |> Result.map (dateTimePartsToValue (Just tz))

            Nothing ->
                ValueParser.parseDateTime line.value
                    |> Result.map (dateTimePartsToValue Nothing)


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
    in
    case maybeTzid of
        Just tz ->
            DateTimeWithTzid
                { year = parts.year
                , month = parts.month
                , day = parts.day
                , hour = parts.hour
                , minute = parts.minute
                , second = parts.second
                , tzid = tz
                }

        Nothing ->
            if parts.isUtc then
                DateTimeUtc dt

            else
                DateTimeLocal dt


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
    , parameters = line.parameters
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
