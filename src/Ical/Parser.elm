module Ical.Parser exposing
    ( parse
    , Calendar, Event
    , DateTimeValue(..), LocalDateTime
    , Status(..), Transparency(..)
    , Organizer, RawProperty
    )

{-| Parse iCal ([RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545)) calendar strings.

@docs parse
@docs Calendar, Event
@docs DateTimeValue, LocalDateTime
@docs Status, Transparency
@docs Organizer, RawProperty

-}

import ContentLine exposing (ContentLine)
import Date
import Dict exposing (Dict)
import Time
import VTimeZone
import ValueParser


{-| A parsed iCal calendar.
-}
type alias Calendar =
    { prodId : String
    , version : String
    , events : List Event
    , extraProperties : List RawProperty
    }


{-| A parsed iCal event.

The `uid`, `stamp`, and `start` fields are required by
[RFC 5545 Section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1).
Parsing will fail if any of these are missing from a VEVENT.

-}
type alias Event =
    { uid : String
    , stamp : Time.Posix
    , start : DateTimeValue
    , end : Maybe DateTimeValue
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , summary : Maybe String
    , description : Maybe String
    , location : Maybe String
    , organizer : Maybe Organizer
    , status : Maybe Status
    , transparency : Maybe Transparency
    , extraProperties : List RawProperty
    }


{-| A parsed date or date-time value.

  - `Date` — an all-day value (iCal `VALUE=DATE`), gives a `Date.Date`
  - `DateTime` — a resolved instant in time (UTC or TZID resolved via VTIMEZONE),
    gives a `Time.Posix`. The `timeZoneName` is `Nothing` for UTC datetimes,
    or a [IANA Time Zone Database](https://www.iana.org/time-zones) name
    (e.g. `Just "America/New_York"`) for TZID-resolved datetimes.
  - `FloatingDateTime` — a local date-time with no timezone; interpreted in the
    viewer's local time. Cannot be resolved to an instant without external timezone info.

-}
type DateTimeValue
    = Date Date.Date
    | DateTime { posix : Time.Posix, timeZoneName : Maybe String }
    | FloatingDateTime LocalDateTime


{-| A date-time without timezone information. Represents a "floating" local
date-time that is interpreted in the viewer's local time.
-}
type alias LocalDateTime =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


{-| Event status per
[RFC 5545 Section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11).
-}
type Status
    = Tentative
    | Confirmed
    | Cancelled


{-| Event transparency per
[RFC 5545 Section 3.8.2.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.7).
-}
type Transparency
    = Opaque
    | Transparent


{-| A parsed ORGANIZER property.
-}
type alias Organizer =
    { email : String
    , name : Maybe String
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
    --> Ok { prodId = "-//My App//EN", version = "2.0", events = [...], ... }

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


type alias ParseState =
    { prodId : Maybe String
    , version : Maybe String
    , events : List Event
    , extraProperties : List RawProperty
    , timezones : Dict String VTimeZone.ZoneDefinition
    }


parseCalendar : List ContentLine -> Result String Calendar
parseCalendar contentLines =
    case contentLines of
        first :: rest ->
            if first.name == "BEGIN" && String.toUpper first.value == "VCALENDAR" then
                parseCalendarBody rest
                    { prodId = Nothing
                    , version = Nothing
                    , events = []
                    , extraProperties = []
                    , timezones = Dict.empty
                    }

            else
                Err "Expected BEGIN:VCALENDAR"

        [] ->
            Err "Empty input"


parseCalendarBody : List ContentLine -> ParseState -> Result String Calendar
parseCalendarBody lines state =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VCALENDAR"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VCALENDAR" then
                case ( state.prodId, state.version ) of
                    ( Just prodId, Just version ) ->
                        Ok
                            { prodId = prodId
                            , version = version
                            , events = List.reverse state.events
                            , extraProperties = List.reverse state.extraProperties
                            }

                    ( Nothing, _ ) ->
                        Err "VCALENDAR missing required PRODID"

                    ( _, Nothing ) ->
                        Err "VCALENDAR missing required VERSION"

            else if line.name == "END" then
                Err ("Mismatched END: expected VCALENDAR, got " ++ line.value)

            else if line.name == "BEGIN" && String.toUpper line.value == "VTIMEZONE" then
                case VTimeZone.parseFromContentLines rest of
                    Ok ( tzid, zoneDef, remaining ) ->
                        parseCalendarBody remaining
                            { state | timezones = Dict.insert tzid zoneDef state.timezones }

                    Err err ->
                        Err err

            else if line.name == "BEGIN" && String.toUpper line.value == "VEVENT" then
                case parseEvent rest state.timezones emptyEventAccum of
                    Ok ( event, remaining ) ->
                        parseCalendarBody remaining { state | events = event :: state.events }

                    Err err ->
                        Err err

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        parseCalendarBody remaining state

                    Err err ->
                        Err err

            else
                case line.name of
                    "VERSION" ->
                        parseCalendarBody rest { state | version = Just line.value }

                    "PRODID" ->
                        parseCalendarBody rest { state | prodId = Just line.value }

                    _ ->
                        parseCalendarBody rest
                            { state
                                | extraProperties =
                                    { name = line.name
                                    , parameters = line.parameters
                                    , value = line.value
                                    }
                                        :: state.extraProperties
                            }



-- Event accumulator (all-Maybe during parsing, validated at END:VEVENT)


type alias EventAccum =
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
    , status : Maybe Status
    , transparency : Maybe Transparency
    , extraProperties : List RawProperty
    }


emptyEventAccum : EventAccum
emptyEventAccum =
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
    , transparency = Nothing
    , extraProperties = []
    }


finalizeEvent : EventAccum -> Result String Event
finalizeEvent accum =
    case ( accum.uid, accum.dtstamp, accum.dtstart ) of
        ( Just uid, Just dtstamp, Just dtstart ) ->
            extractPosix "DTSTAMP" dtstamp
                |> Result.andThen
                    (\stamp ->
                        extractMaybePosix "CREATED" accum.created
                            |> Result.andThen
                                (\created ->
                                    extractMaybePosix "LAST-MODIFIED" accum.lastModified
                                        |> Result.map
                                            (\lastModified ->
                                                { uid = uid
                                                , stamp = stamp
                                                , start = dtstart
                                                , end = accum.dtend
                                                , created = created
                                                , lastModified = lastModified
                                                , summary = accum.summary
                                                , description = accum.description
                                                , location = accum.location
                                                , organizer = accum.organizer
                                                , status = accum.status
                                                , transparency = accum.transparency
                                                , extraProperties = List.reverse accum.extraProperties
                                                }
                                            )
                                )
                    )

        ( Nothing, _, _ ) ->
            Err "VEVENT missing required UID"

        ( _, Nothing, _ ) ->
            Err "VEVENT missing required DTSTAMP"

        ( _, _, Nothing ) ->
            Err "VEVENT missing required DTSTART"


extractPosix : String -> DateTimeValue -> Result String Time.Posix
extractPosix fieldName dtv =
    case dtv of
        DateTime { posix } ->
            Ok posix

        _ ->
            Err (fieldName ++ " must be a UTC datetime")


extractMaybePosix : String -> Maybe DateTimeValue -> Result String (Maybe Time.Posix)
extractMaybePosix fieldName maybeDtv =
    case maybeDtv of
        Nothing ->
            Ok Nothing

        Just (DateTime { posix }) ->
            Ok (Just posix)

        Just _ ->
            Err (fieldName ++ " must be a UTC datetime")


parseEvent : List ContentLine -> Dict String VTimeZone.ZoneDefinition -> EventAccum -> Result String ( Event, List ContentLine )
parseEvent lines timezones accum =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VEVENT"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VEVENT" then
                finalizeEvent accum
                    |> Result.map (\event -> ( event, rest ))

            else if line.name == "END" then
                Err ("Mismatched END: expected VEVENT, got " ++ line.value)

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        parseEvent remaining timezones accum

                    Err err ->
                        Err err

            else
                parseEvent rest timezones (applyEventProperty timezones line accum)


applyEventProperty : Dict String VTimeZone.ZoneDefinition -> ContentLine -> EventAccum -> EventAccum
applyEventProperty timezones line accum =
    let
        rawProp : RawProperty
        rawProp =
            { name = line.name
            , parameters = line.parameters
            , value = line.value
            }
    in
    case line.name of
        "UID" ->
            { accum | uid = Just line.value }

        "DTSTAMP" ->
            { accum | dtstamp = parseDateTimeValue timezones line |> Result.toMaybe }

        "DTSTART" ->
            { accum | dtstart = parseDateTimeValue timezones line |> Result.toMaybe }

        "DTEND" ->
            { accum | dtend = parseDateTimeValue timezones line |> Result.toMaybe }

        "CREATED" ->
            { accum | created = parseDateTimeValue timezones line |> Result.toMaybe }

        "LAST-MODIFIED" ->
            { accum | lastModified = parseDateTimeValue timezones line |> Result.toMaybe }

        "SUMMARY" ->
            { accum | summary = Just (ValueParser.unescapeText line.value) }

        "DESCRIPTION" ->
            { accum | description = Just (ValueParser.unescapeText line.value) }

        "LOCATION" ->
            { accum | location = Just (ValueParser.unescapeText line.value) }

        "ORGANIZER" ->
            { accum | organizer = Just (parseOrganizer line) }

        "STATUS" ->
            case parseStatus line.value of
                Just s ->
                    { accum | status = Just s }

                Nothing ->
                    { accum | extraProperties = rawProp :: accum.extraProperties }

        "TRANSP" ->
            case parseTransparency line.value of
                Just t ->
                    { accum | transparency = Just t }

                Nothing ->
                    { accum | extraProperties = rawProp :: accum.extraProperties }

        _ ->
            { accum | extraProperties = rawProp :: accum.extraProperties }


parseStatus : String -> Maybe Status
parseStatus value =
    case String.toUpper value of
        "TENTATIVE" ->
            Just Tentative

        "CONFIRMED" ->
            Just Confirmed

        "CANCELLED" ->
            Just Cancelled

        _ ->
            Nothing


parseTransparency : String -> Maybe Transparency
parseTransparency value =
    case String.toUpper value of
        "OPAQUE" ->
            Just Opaque

        "TRANSPARENT" ->
            Just Transparent

        _ ->
            Nothing


parseDateTimeValue : Dict String VTimeZone.ZoneDefinition -> ContentLine -> Result String DateTimeValue
parseDateTimeValue timezones line =
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
            |> Result.map
                (\{ year, month, day } ->
                    Date (Date.fromCalendarDate year (intToMonth month) day)
                )

    else
        ValueParser.parseDateTime line.value
            |> Result.andThen (dateTimePartsToValue timezones tzid)


dateTimePartsToValue : Dict String VTimeZone.ZoneDefinition -> Maybe String -> ValueParser.DateTimeParts -> Result String DateTimeValue
dateTimePartsToValue timezones maybeTzid parts =
    let
        dt : LocalDateTime
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
            case Dict.get tz timezones of
                Just zoneDef ->
                    VTimeZone.resolve zoneDef dt
                        |> Result.map (\posix -> DateTime { posix = posix, timeZoneName = Just tz })

                Nothing ->
                    -- No VTIMEZONE found for this TZID, keep as FloatingDateTime
                    Ok (FloatingDateTime dt)

        Nothing ->
            if parts.isUtc then
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate parts.year (intToMonth parts.month) parts.day

                    daysSinceEpoch : Int
                    daysSinceEpoch =
                        Date.toRataDie date - 719163

                    totalSeconds : Int
                    totalSeconds =
                        daysSinceEpoch * 86400 + parts.hour * 3600 + parts.minute * 60 + parts.second
                in
                Ok (DateTime { posix = Time.millisToPosix (totalSeconds * 1000), timeZoneName = Nothing })

            else
                Ok
                    (FloatingDateTime dt)


intToMonth : Int -> Time.Month
intToMonth m =
    case m of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec


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

        email : String
        email =
            if String.startsWith "mailto:" (String.toLower line.value) then
                String.dropLeft 7 line.value

            else
                line.value
    in
    { email = email
    , name = cn
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
