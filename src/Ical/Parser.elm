module Ical.Parser exposing
    ( parse
    , Calendar, Event
    , EventTime(..), ResolvedTime, LocalDateTime
    , Status(..), Transparency(..)
    , Organizer, RawProperty
    , Attendee, AttendeeRole(..), ParticipationStatus(..)
    )

{-| Parse iCal ([RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545)) calendar strings.

@docs parse
@docs Calendar, Event
@docs EventTime, ResolvedTime, LocalDateTime
@docs Status, Transparency
@docs Organizer, RawProperty
@docs Attendee, AttendeeRole, ParticipationStatus

-}

import ContentLine exposing (ContentLine)
import Date
import Dict exposing (Dict)
import Ical.Recurrence exposing (RecurrenceRule)
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

The `uid`, `stamp`, and `time` fields are required by
[RFC 5545 Section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1).
Parsing will fail if DTSTART is missing from a VEVENT.

The `end` inside `time` is populated from either DTEND or DURATION (which are
[mutually exclusive](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1)
per the spec). When neither is present, `end` is `Nothing`.

-}
type alias Event =
    { uid : String
    , stamp : Time.Posix
    , time : EventTime
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , summary : Maybe String
    , description : Maybe String
    , location : Maybe String
    , organizer : Maybe Organizer
    , status : Maybe Status
    , transparency : Maybe Transparency
    , recurrenceRules : List RecurrenceRule
    , exclusions : List Time.Posix
    , attendees : List Attendee
    , extraProperties : List RawProperty
    }


{-| The time span of an event. The variant determines what type of values
`start` and `end` carry, enforcing the
[RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.2)
requirement that DTSTART and DTEND share the same value type.

  - `AllDay` — `VALUE=DATE`; the event spans whole days.
  - `WithTime` — a resolved instant in time (UTC or TZID resolved via VTIMEZONE).
  - `FloatingTime` — a local date-time with no timezone; interpreted in the
    viewer's local time.

-}
type EventTime
    = AllDay { start : Date.Date, end : Maybe Date.Date }
    | WithTime { start : ResolvedTime, end : Maybe ResolvedTime }
    | FloatingTime { start : LocalDateTime, end : Maybe LocalDateTime }


{-| A resolved date-time instant. The `timeZoneName` is `Nothing` for UTC
datetimes, or a [IANA Time Zone Database](https://www.iana.org/time-zones)
name (e.g. `Just "America/New_York"`) for TZID-resolved datetimes.
-}
type alias ResolvedTime =
    { posix : Time.Posix
    , timeZoneName : Maybe String
    }


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


{-| A parsed ATTENDEE property.
-}
type alias Attendee =
    { email : String
    , name : Maybe String
    , role : AttendeeRole
    , participationStatus : ParticipationStatus
    , rsvp : Bool
    }


{-| Attendee role per
[RFC 5545 Section 3.2.16](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.16).
-}
type AttendeeRole
    = Chair
    | Required
    | Optional
    | NonParticipant


{-| Attendee participation status per
[RFC 5545 Section 3.2.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.12).
-}
type ParticipationStatus
    = NeedsAction
    | Accepted
    | Declined
    | TentativeParticipation
    | Delegated


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
-- Internally we still use DateTimeValue to track the raw parsed form of
-- dtstart/dtend before building the final EventTime.


type InternalDateTimeValue
    = IDate Date.Date
    | IDateTime ResolvedTime
    | IFloating LocalDateTime


type alias EventAccum =
    { uid : Maybe String
    , dtstamp : Maybe InternalDateTimeValue
    , dtstart : Maybe InternalDateTimeValue
    , dtend : Maybe InternalDateTimeValue
    , duration : Maybe ValueParser.Duration
    , created : Maybe InternalDateTimeValue
    , lastModified : Maybe InternalDateTimeValue
    , summary : Maybe String
    , description : Maybe String
    , location : Maybe String
    , organizer : Maybe Organizer
    , status : Maybe Status
    , transparency : Maybe Transparency
    , recurrenceRules : List RecurrenceRule
    , exclusions : List Time.Posix
    , attendees : List Attendee
    , extraProperties : List RawProperty
    }


emptyEventAccum : EventAccum
emptyEventAccum =
    { uid = Nothing
    , dtstamp = Nothing
    , dtstart = Nothing
    , dtend = Nothing
    , duration = Nothing
    , created = Nothing
    , lastModified = Nothing
    , summary = Nothing
    , description = Nothing
    , location = Nothing
    , organizer = Nothing
    , status = Nothing
    , transparency = Nothing
    , recurrenceRules = []
    , exclusions = []
    , attendees = []
    , extraProperties = []
    }


finalizeEvent : EventAccum -> Result String Event
finalizeEvent accum =
    case ( accum.uid, accum.dtstamp, accum.dtstart ) of
        ( Just uid, Just dtstamp, Just dtstart ) ->
            extractPosix "DTSTAMP" dtstamp
                |> Result.andThen
                    (\stamp ->
                        buildEventTime dtstart accum.dtend accum.duration
                            |> Result.andThen
                                (\time ->
                                    extractMaybePosix "CREATED" accum.created
                                        |> Result.andThen
                                            (\created ->
                                                extractMaybePosix "LAST-MODIFIED" accum.lastModified
                                                    |> Result.map
                                                        (\lastModified ->
                                                            { uid = uid
                                                            , stamp = stamp
                                                            , time = time
                                                            , created = created
                                                            , lastModified = lastModified
                                                            , summary = accum.summary
                                                            , description = accum.description
                                                            , location = accum.location
                                                            , organizer = accum.organizer
                                                            , status = accum.status
                                                            , transparency = accum.transparency
                                                            , recurrenceRules = List.reverse accum.recurrenceRules
                                                            , exclusions = List.reverse accum.exclusions
                                                            , attendees = List.reverse accum.attendees
                                                            , extraProperties = List.reverse accum.extraProperties
                                                            }
                                                        )
                                            )
                                )
                    )

        ( Nothing, _, _ ) ->
            Err "VEVENT missing required UID"

        ( _, Nothing, _ ) ->
            Err "VEVENT missing required DTSTAMP"

        ( _, _, Nothing ) ->
            Err "VEVENT missing required DTSTART"


buildEventTime : InternalDateTimeValue -> Maybe InternalDateTimeValue -> Maybe ValueParser.Duration -> Result String EventTime
buildEventTime dtstart maybeDtend maybeDuration =
    case dtstart of
        IDate startDate ->
            case maybeDtend of
                Just (IDate endDate) ->
                    Ok (AllDay { start = startDate, end = Just endDate })

                Just _ ->
                    Err "DTEND value type must match DTSTART (expected DATE)"

                Nothing ->
                    case maybeDuration of
                        Just dur ->
                            Ok (AllDay { start = startDate, end = Just (addDurationToDate dur startDate) })

                        Nothing ->
                            Ok (AllDay { start = startDate, end = Nothing })

        IDateTime startResolved ->
            case maybeDtend of
                Just (IDateTime endResolved) ->
                    Ok (WithTime { start = startResolved, end = Just endResolved })

                Just _ ->
                    Err "DTEND value type must match DTSTART (expected DATE-TIME)"

                Nothing ->
                    case maybeDuration of
                        Just dur ->
                            Ok (WithTime { start = startResolved, end = Just (addDurationToResolved dur startResolved) })

                        Nothing ->
                            Ok (WithTime { start = startResolved, end = Nothing })

        IFloating startLocal ->
            case maybeDtend of
                Just (IFloating endLocal) ->
                    Ok (FloatingTime { start = startLocal, end = Just endLocal })

                Just _ ->
                    Err "DTEND value type must match DTSTART (expected local DATE-TIME)"

                Nothing ->
                    case maybeDuration of
                        Just dur ->
                            Ok (FloatingTime { start = startLocal, end = Just (addDurationToLocal dur startLocal) })

                        Nothing ->
                            Ok (FloatingTime { start = startLocal, end = Nothing })


addDurationToDate : ValueParser.Duration -> Date.Date -> Date.Date
addDurationToDate dur date =
    let
        totalDays : Int
        totalDays =
            dur.weeks * 7 + dur.days
    in
    Date.add Date.Days totalDays date


addDurationToResolved : ValueParser.Duration -> ResolvedTime -> ResolvedTime
addDurationToResolved dur resolved =
    let
        totalSeconds : Int
        totalSeconds =
            dur.weeks * 7 * 86400 + dur.days * 86400 + dur.hours * 3600 + dur.minutes * 60 + dur.seconds

        millis : Int
        millis =
            Time.posixToMillis resolved.posix + totalSeconds * 1000
    in
    { posix = Time.millisToPosix millis
    , timeZoneName = resolved.timeZoneName
    }


addDurationToLocal : ValueParser.Duration -> LocalDateTime -> LocalDateTime
addDurationToLocal dur dt =
    let
        totalSeconds : Int
        totalSeconds =
            dur.hours * 3600 + dur.minutes * 60 + dur.seconds

        -- Convert local datetime to a date + seconds-of-day, add duration, convert back
        date : Date.Date
        date =
            Date.fromCalendarDate dt.year (intToMonth dt.month) dt.day

        secondsOfDay : Int
        secondsOfDay =
            dt.hour * 3600 + dt.minute * 60 + dt.second + totalSeconds

        extraDays : Int
        extraDays =
            dur.weeks * 7 + dur.days + (secondsOfDay // 86400)

        remainingSeconds : Int
        remainingSeconds =
            modBy 86400 secondsOfDay

        newDate : Date.Date
        newDate =
            Date.add Date.Days extraDays date
    in
    { year = Date.year newDate
    , month = Date.monthNumber newDate
    , day = Date.day newDate
    , hour = remainingSeconds // 3600
    , minute = modBy 60 (remainingSeconds // 60)
    , second = modBy 60 remainingSeconds
    }


extractPosix : String -> InternalDateTimeValue -> Result String Time.Posix
extractPosix fieldName dtv =
    case dtv of
        IDateTime { posix } ->
            Ok posix

        _ ->
            Err (fieldName ++ " must be a UTC datetime")


extractMaybePosix : String -> Maybe InternalDateTimeValue -> Result String (Maybe Time.Posix)
extractMaybePosix fieldName maybeDtv =
    case maybeDtv of
        Nothing ->
            Ok Nothing

        Just (IDateTime { posix }) ->
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

        "DURATION" ->
            { accum | duration = ValueParser.parseDuration line.value |> Result.toMaybe }

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

        "RRULE" ->
            case ValueParser.parseRecurrenceRule line.value of
                Ok rule ->
                    { accum | recurrenceRules = rule :: accum.recurrenceRules }

                Err _ ->
                    { accum | extraProperties = rawProp :: accum.extraProperties }

        "EXDATE" ->
            { accum | exclusions = List.reverse (parseExdateValues timezones line) ++ accum.exclusions }

        "ATTENDEE" ->
            { accum | attendees = parseAttendee line :: accum.attendees }

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


parseDateTimeValue : Dict String VTimeZone.ZoneDefinition -> ContentLine -> Result String InternalDateTimeValue
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
                    IDate (Date.fromCalendarDate year (intToMonth month) day)
                )

    else
        ValueParser.parseDateTime line.value
            |> Result.andThen (dateTimePartsToValue timezones tzid)


dateTimePartsToValue : Dict String VTimeZone.ZoneDefinition -> Maybe String -> ValueParser.DateTimeParts -> Result String InternalDateTimeValue
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
                        |> Result.map (\posix -> IDateTime { posix = posix, timeZoneName = Just tz })

                Nothing ->
                    -- No VTIMEZONE found for this TZID, keep as floating
                    Ok (IFloating dt)

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
                Ok (IDateTime { posix = Time.millisToPosix (totalSeconds * 1000), timeZoneName = Nothing })

            else
                Ok (IFloating dt)


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


parseAttendee : ContentLine -> Attendee
parseAttendee line =
    let
        getParam : String -> Maybe String
        getParam key =
            line.parameters
                |> List.filterMap
                    (\( k, v ) ->
                        if String.toUpper k == key then
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

        role : AttendeeRole
        role =
            case Maybe.map String.toUpper (getParam "ROLE") of
                Just "CHAIR" ->
                    Chair

                Just "OPT-PARTICIPANT" ->
                    Optional

                Just "NON-PARTICIPANT" ->
                    NonParticipant

                _ ->
                    Required

        partStat : ParticipationStatus
        partStat =
            case Maybe.map String.toUpper (getParam "PARTSTAT") of
                Just "ACCEPTED" ->
                    Accepted

                Just "DECLINED" ->
                    Declined

                Just "TENTATIVE" ->
                    TentativeParticipation

                Just "DELEGATED" ->
                    Delegated

                _ ->
                    NeedsAction

        rsvp : Bool
        rsvp =
            case Maybe.map String.toUpper (getParam "RSVP") of
                Just "TRUE" ->
                    True

                _ ->
                    False
    in
    { email = email
    , name = getParam "CN"
    , role = role
    , participationStatus = partStat
    , rsvp = rsvp
    }


parseExdateValues : Dict String VTimeZone.ZoneDefinition -> ContentLine -> List Time.Posix
parseExdateValues timezones line =
    String.split "," line.value
        |> List.filterMap
            (\val ->
                let
                    fakeLine : ContentLine
                    fakeLine =
                        { name = "EXDATE"
                        , parameters = line.parameters
                        , value = val
                        }
                in
                case parseDateTimeValue timezones fakeLine of
                    Ok (IDateTime { posix }) ->
                        Just posix

                    _ ->
                        Nothing
            )


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
