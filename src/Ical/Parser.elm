module Ical.Parser exposing
    ( parse
    , Calendar, Event
    , EventTime(..), ResolvedTime, LocalDateTime
    , Status(..), Transparency(..)
    , Organizer, RawProperty
    , Attendee, AttendeeRole(..), ParticipationStatus(..)
    , Occurrence, expand, expandNext
    )

{-| Parse iCal ([RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545)) calendar strings
into typed Elm data.

    import Date
    import Ical.Parser as Parser

    holidayFeedToString : String -> String
    holidayFeedToString icsString =
        case Parser.parse icsString of
            Ok cal ->
                cal.events
                    |> List.filterMap toHoliday
                    |> List.sortBy
                        (.date >> Date.toRataDie)
                    |> List.map
                        (\h ->
                            Date.format "EEE, MMM d" h.date
                                ++ " - "
                                ++ h.name
                        )
                    |> String.join "\n"

            Err err ->
                "Parse error: " ++ err

    toHoliday :
        Parser.Event
        -> Maybe { name : String, date : Date.Date }
    toHoliday event =
        case ( event.summary, event.time ) of
            ( Just name, Parser.AllDay { start } ) ->
                Just { name = name, date = start }

            _ ->
                Nothing

    -- holidayFeedToString usHolidaysFeed
    --
    --   Thu, Jan 1 - New Year's Day
    --   Mon, Jan 19 - MLK Day
    --   ...

See the [`examples/` folder](https://github.com/dillonkearns/elm-ical/tree/main/examples)
for a full runnable script that fetches and parses a live Google Calendar feed.

Parsing is strict: malformed content lines, invalid dates and times, bad
`RRULE` values, and a `TZID` without a matching `VTIMEZONE` all return `Err`.
Parsed types are transparent record aliases so you can read fields directly.


## Parsing

@docs parse
@docs Calendar, Event


## Event times

@docs EventTime, ResolvedTime, LocalDateTime


## Enums and metadata

@docs Status, Transparency
@docs Organizer, RawProperty
@docs Attendee, AttendeeRole, ParticipationStatus


## Recurrence expansion

@docs Occurrence, expand, expandNext

-}

import ContentLine exposing (ContentLine)
import Date
import Dict exposing (Dict)
import Ical.Recurrence exposing (DaySpec, Frequency(..), RecurrenceEnd(..), RecurrenceRule)
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
per the spec). When neither is present, the parser applies the RFC default:

  - DATE events end on the following date
  - DATE-TIME events end at the same instant they start

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

  - `AllDay`: `VALUE=DATE`; the event spans whole days.
  - `WithTime`: a resolved instant in time (UTC or TZID resolved via VTIMEZONE).
  - `FloatingTime`: a local date-time with no timezone; interpreted in the
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


{-| An uninterpreted property: name, parameters, and raw value string.
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
    parseContentLines lines []
        |> Result.andThen parseCalendar


parseContentLines : List String -> List ContentLine -> Result String (List ContentLine)
parseContentLines remaining acc =
    case remaining of
        [] ->
            Ok (List.reverse acc)

        line :: rest ->
            case ContentLine.parse line of
                Ok contentLine ->
                    parseContentLines rest (contentLine :: acc)

                Err err ->
                    Err err


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
    , errors : List String
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
    , errors = []
    }


finalizeEvent : EventAccum -> Result String Event
finalizeEvent accum =
    case List.reverse accum.errors of
        firstError :: _ ->
            Err firstError

        [] ->
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
    case ( maybeDtend, maybeDuration ) of
        ( Just _, Just _ ) ->
            Err "VEVENT must not contain both DTEND and DURATION"

        _ ->
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
                                    Ok (AllDay { start = startDate, end = Just (Date.add Date.Days 1 startDate) })

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
                                    Ok (WithTime { start = startResolved, end = Just startResolved })

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
                                    Ok (FloatingTime { start = startLocal, end = Just startLocal })


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
            Date.fromCalendarDate dt.year (Date.numberToMonth dt.month) dt.day

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
            case parseDateTimeValue timezones line of
                Ok dtstamp ->
                    { accum | dtstamp = Just dtstamp }

                Err err ->
                    addEventError ("Invalid DTSTAMP: " ++ err) accum

        "DTSTART" ->
            case parseDateTimeValue timezones line of
                Ok dtstart ->
                    { accum | dtstart = Just dtstart }

                Err err ->
                    addEventError ("Invalid DTSTART: " ++ err) accum

        "DTEND" ->
            case parseDateTimeValue timezones line of
                Ok dtend ->
                    { accum | dtend = Just dtend }

                Err err ->
                    addEventError ("Invalid DTEND: " ++ err) accum

        "DURATION" ->
            case ValueParser.parseDuration line.value of
                Ok duration ->
                    { accum | duration = Just duration }

                Err err ->
                    addEventError ("Invalid DURATION: " ++ err) accum

        "CREATED" ->
            case parseDateTimeValue timezones line of
                Ok created ->
                    { accum | created = Just created }

                Err err ->
                    addEventError ("Invalid CREATED: " ++ err) accum

        "LAST-MODIFIED" ->
            case parseDateTimeValue timezones line of
                Ok lastModified ->
                    { accum | lastModified = Just lastModified }

                Err err ->
                    addEventError ("Invalid LAST-MODIFIED: " ++ err) accum

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

                Err err ->
                    addEventError ("Invalid RRULE: " ++ err) accum

        "EXDATE" ->
            { accum | exclusions = List.reverse (parseExdateValues timezones line) ++ accum.exclusions }

        "ATTENDEE" ->
            { accum | attendees = parseAttendee line :: accum.attendees }

        _ ->
            { accum | extraProperties = rawProp :: accum.extraProperties }


addEventError : String -> EventAccum -> EventAccum
addEventError error accum =
    { accum | errors = error :: accum.errors }


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
                    IDate (Date.fromCalendarDate year (Date.numberToMonth month) day)
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
                    Err ("TZID \"" ++ tz ++ "\" has no matching VTIMEZONE definition")

        Nothing ->
            if parts.isUtc then
                let
                    date : Date.Date
                    date =
                        Date.fromCalendarDate parts.year (Date.numberToMonth parts.month) parts.day

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
                    Ok (IDate date) ->
                        Just (dateToUtcMidnight date)

                    Ok (IDateTime { posix }) ->
                        Just posix

                    Ok (IFloating localDateTime) ->
                        Just
                            (dateToUtcMidnight
                                (Date.fromCalendarDate localDateTime.year (Date.numberToMonth localDateTime.month) localDateTime.day)
                            )

                    _ ->
                        Nothing
            )


dateToUtcMidnight : Date.Date -> Time.Posix
dateToUtcMidnight date =
    Time.millisToPosix ((Date.toRataDie date - 719163) * 86400 * 1000)


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



-- RECURRENCE EXPANSION


{-| A single occurrence of an event. For non-recurring events, there is one
occurrence whose `time` matches `event.time`. For recurring events, each
occurrence has its own `time` shifted to the recurrence date while preserving
the original event's duration.
-}
type alias Occurrence =
    { event : Event
    , time : EventTime
    }


{-| Expand an event's recurrence rules into a list of concrete occurrences
within a date range.

    expand { start = jan1, end = dec31 } event

For events without recurrence rules, returns a single occurrence if the event
falls within the range. For events with recurrence rules, generates all
occurrences within the range, respecting COUNT, UNTIL, EXDATE, and all BY\*
filters.

-}
expand : { start : Date.Date, end : Date.Date } -> Event -> List Occurrence
expand range event =
    case event.recurrenceRules of
        [] ->
            let
                eventDate : Date.Date
                eventDate =
                    occurrenceStartDate event.time
            in
            if Date.compare eventDate range.start /= LT && Date.compare eventDate range.end /= GT then
                [ { event = event, time = event.time } ]

            else
                []

        rules ->
            rules
                |> List.concatMap (\rule -> expandRule range event rule)
                |> List.sortBy (\occ -> Date.toRataDie (occurrenceStartDate occ.time))
                |> dedupOccurrences


{-| Get the next N occurrences of an event starting from a given date.

    expandNext 5 today event

For non-recurring events, returns a single occurrence if the event falls on or
after the start date, or an empty list if it is before.

-}
expandNext : Int -> Date.Date -> Event -> List Occurrence
expandNext n fromDate event =
    case event.recurrenceRules of
        [] ->
            let
                eventDate : Date.Date
                eventDate =
                    occurrenceStartDate event.time
            in
            if Date.compare eventDate fromDate /= LT then
                [ { event = event, time = event.time } ]

            else
                []

        rules ->
            rules
                |> List.concatMap (\rule -> expandNextRule n fromDate event rule)
                |> List.sortBy (\occ -> Date.toRataDie (occurrenceStartDate occ.time))
                |> dedupOccurrences
                |> List.take n


expandNextRule : Int -> Date.Date -> Event -> RecurrenceRule -> List Occurrence
expandNextRule n fromDate event rule =
    let
        seed : Date.Date
        seed =
            occurrenceStartDate event.time

        fromRD : Int
        fromRD =
            Date.toRataDie fromDate
    in
    expandNextChunked rule seed fromRD n event 1 []


expandNextChunked : RecurrenceRule -> Date.Date -> Int -> Int -> Event -> Int -> List Occurrence -> List Occurrence
expandNextChunked rule seed fromRD needed event chunkYears acc =
    let
        rangeEndRD : Int
        rangeEndRD =
            fromRD + (chunkYears * 366)

        candidates : List Date.Date
        candidates =
            generateCandidates rule event.time seed rangeEndRD

        filtered : List Date.Date
        filtered =
            candidates
                |> filterExclusions event

        newOccurrences : List Occurrence
        newOccurrences =
            filtered
                |> List.filter (\d -> Date.toRataDie d >= fromRD)
                |> List.map (\d -> { event = event, time = shiftTime event.time seed d })

        combined : List Occurrence
        combined =
            acc ++ newOccurrences
    in
    if List.length combined >= needed then
        List.take needed combined

    else
        case rule.end of
            Forever ->
                if chunkYears >= 100 then
                    combined

                else
                    expandNextChunked rule seed fromRD needed event (chunkYears * 2) []

            _ ->
                combined


expandRule : { start : Date.Date, end : Date.Date } -> Event -> RecurrenceRule -> List Occurrence
expandRule range event rule =
    let
        seed : Date.Date
        seed =
            occurrenceStartDate event.time

        rangeStartRD : Int
        rangeStartRD =
            Date.toRataDie range.start

        rangeEndRD : Int
        rangeEndRD =
            Date.toRataDie range.end

        candidates : List Date.Date
        candidates =
            generateCandidates rule event.time seed rangeEndRD

        filtered : List Date.Date
        filtered =
            candidates
                |> filterExclusions event
    in
    filtered
        |> List.filter (\d -> Date.toRataDie d >= rangeStartRD && Date.toRataDie d <= rangeEndRD)
        |> List.map (\d -> { event = event, time = shiftTime event.time seed d })


generateCandidates : RecurrenceRule -> EventTime -> Date.Date -> Int -> List Date.Date
generateCandidates rule originalTime seed rangeEndRD =
    generateLoop rule originalTime seed rangeEndRD 0 0 []


generateLoop : RecurrenceRule -> EventTime -> Date.Date -> Int -> Int -> Int -> List Date.Date -> List Date.Date
generateLoop rule originalTime seed rangeEndRD stepIndex emittedCount acc =
    let
        intervalDate : Date.Date
        intervalDate =
            advanceByInterval rule.frequency (rule.interval * stepIndex) seed

        candidates : List Date.Date
        candidates =
            expandWithinPeriod rule seed intervalDate
                |> applyBySetPos rule.bySetPos
    in
    if List.isEmpty candidates then
        if Date.toRataDie intervalDate > rangeEndRD + 366 then
            List.reverse acc

        else
            generateLoop rule originalTime seed rangeEndRD (stepIndex + 1) emittedCount acc

    else
        let
            lowestCandidateRD : Int
            lowestCandidateRD =
                candidates
                    |> List.map Date.toRataDie
                    |> List.minimum
                    |> Maybe.withDefault (rangeEndRD + 1)
        in
        if lowestCandidateRD > rangeEndRD && not (isCountLimited rule.end) then
            List.reverse acc

        else
            collectCandidates rule originalTime seed rangeEndRD stepIndex emittedCount acc candidates


collectCandidates : RecurrenceRule -> EventTime -> Date.Date -> Int -> Int -> Int -> List Date.Date -> List Date.Date -> List Date.Date
collectCandidates rule originalTime seed rangeEndRD stepIndex emittedCount acc candidates =
    case candidates of
        [] ->
            generateLoop rule originalTime seed rangeEndRD (stepIndex + 1) emittedCount acc

        c :: rest ->
            if Date.toRataDie c < Date.toRataDie seed then
                collectCandidates rule originalTime seed rangeEndRD stepIndex emittedCount acc rest

            else
                case rule.end of
                    Count n ->
                        if emittedCount >= n then
                            List.reverse acc

                        else
                            collectCandidates rule originalTime seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    UntilDate untilDate ->
                        if Date.compare c untilDate == GT then
                            List.reverse acc

                        else
                            collectCandidates rule originalTime seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    UntilDateTime untilPosix ->
                        if occurrenceStartsAfterUntil originalTime seed c untilPosix then
                            List.reverse acc

                        else
                            collectCandidates rule originalTime seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    Forever ->
                        if Date.toRataDie c > rangeEndRD then
                            List.reverse acc

                        else
                            collectCandidates rule originalTime seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest


occurrenceStartsAfterUntil : EventTime -> Date.Date -> Date.Date -> Time.Posix -> Bool
occurrenceStartsAfterUntil originalTime seed candidateDate untilPosix =
    case shiftTime originalTime seed candidateDate of
        WithTime { start } ->
            Time.posixToMillis start.posix > Time.posixToMillis untilPosix

        AllDay { start } ->
            Date.compare start (Date.fromPosix Time.utc untilPosix) == GT

        FloatingTime { start } ->
            Date.compare
                (Date.fromCalendarDate start.year (Date.numberToMonth start.month) start.day)
                (Date.fromPosix Time.utc untilPosix)
                == GT


isCountLimited : RecurrenceEnd -> Bool
isCountLimited end =
    case end of
        Count _ ->
            True

        _ ->
            False


advanceByInterval : Frequency -> Int -> Date.Date -> Date.Date
advanceByInterval freq n seed =
    case freq of
        Daily ->
            Date.add Date.Days n seed

        Weekly ->
            Date.add Date.Weeks n seed

        Monthly ->
            Date.add Date.Months n seed

        Yearly ->
            Date.add Date.Years n seed


expandWithinPeriod : RecurrenceRule -> Date.Date -> Date.Date -> List Date.Date
expandWithinPeriod rule seed intervalDate =
    case rule.frequency of
        Daily ->
            [ intervalDate ]
                |> filterByMonth rule.byMonth
                |> filterByMonthDay rule.byMonthDay
                |> filterByDay rule.byDay

        Weekly ->
            if List.isEmpty rule.byDay then
                [ intervalDate ]
                    |> filterByMonth rule.byMonth

            else
                expandWeekByDay rule.weekStart rule.byDay intervalDate
                    |> filterByMonth rule.byMonth

        Monthly ->
            expandMonthly rule intervalDate

        Yearly ->
            expandYearly rule seed intervalDate


expandWeekByDay : Time.Weekday -> List DaySpec -> Date.Date -> List Date.Date
expandWeekByDay weekStart byDay date =
    let
        weekStartDate : Date.Date
        weekStartDate =
            Date.floor (weekdayToInterval weekStart) date
    in
    byDay
        |> List.filterMap
            (\spec ->
                let
                    target : Date.Date
                    target =
                        Date.ceiling (weekdayToInterval spec.weekday) weekStartDate
                in
                if Date.toRataDie target - Date.toRataDie weekStartDate < 7 then
                    Just target

                else
                    Nothing
            )
        |> List.sortBy Date.toRataDie


expandMonthly : RecurrenceRule -> Date.Date -> List Date.Date
expandMonthly rule intervalDate =
    let
        year : Int
        year =
            Date.year intervalDate

        month : Time.Month
        month =
            Date.month intervalDate

        baseDates : List Date.Date
        baseDates =
            if not (List.isEmpty rule.byMonthDay) then
                expandByMonthDay year month rule.byMonthDay

            else if not (List.isEmpty rule.byDay) then
                expandByDayInMonth year month rule.byDay

            else
                [ intervalDate ]
    in
    baseDates
        |> filterByMonth rule.byMonth
        |> List.sortBy Date.toRataDie


expandYearly : RecurrenceRule -> Date.Date -> Date.Date -> List Date.Date
expandYearly rule seed intervalDate =
    let
        year : Int
        year =
            Date.year intervalDate

        months : List Time.Month
        months =
            if List.isEmpty rule.byMonth then
                [ Date.month seed ]

            else
                rule.byMonth

        baseDates : List Date.Date
        baseDates =
            if not (List.isEmpty rule.byMonthDay) then
                months
                    |> List.concatMap (\m -> expandByMonthDay year m rule.byMonthDay)

            else if not (List.isEmpty rule.byDay) then
                months
                    |> List.concatMap (\m -> expandByDayInMonth year m rule.byDay)

            else
                months
                    |> List.map (\m -> Date.fromCalendarDate year m (Date.day seed))
    in
    baseDates
        |> List.sortBy Date.toRataDie


expandByMonthDay : Int -> Time.Month -> List Int -> List Date.Date
expandByMonthDay year month days =
    let
        maxDay : Int
        maxDay =
            daysInMonth year month
    in
    days
        |> List.filterMap
            (\d ->
                let
                    actualDay : Int
                    actualDay =
                        if d < 0 then
                            maxDay + d + 1

                        else
                            d
                in
                if actualDay >= 1 && actualDay <= maxDay then
                    Just (Date.fromCalendarDate year month actualDay)

                else
                    Nothing
            )


expandByDayInMonth : Int -> Time.Month -> List DaySpec -> List Date.Date
expandByDayInMonth year month specs =
    specs
        |> List.concatMap (resolveDaySpecInMonth year month)
        |> List.sortBy Date.toRataDie


resolveDaySpecInMonth : Int -> Time.Month -> DaySpec -> List Date.Date
resolveDaySpecInMonth year month spec =
    let
        interval : Date.Interval
        interval =
            weekdayToInterval spec.weekday
    in
    case spec.ordinal of
        Just n ->
            if n > 0 then
                let
                    firstOfMonth : Date.Date
                    firstOfMonth =
                        Date.fromCalendarDate year month 1

                    firstMatch : Date.Date
                    firstMatch =
                        Date.ceiling interval firstOfMonth

                    result : Date.Date
                    result =
                        Date.add Date.Weeks (n - 1) firstMatch
                in
                if Date.month result == month then
                    [ result ]

                else
                    []

            else
                let
                    maxDay : Int
                    maxDay =
                        daysInMonth year month

                    lastOfMonth : Date.Date
                    lastOfMonth =
                        Date.fromCalendarDate year month maxDay

                    lastMatch : Date.Date
                    lastMatch =
                        Date.floor interval lastOfMonth

                    result : Date.Date
                    result =
                        Date.add Date.Weeks (n + 1) lastMatch
                in
                if Date.month result == month then
                    [ result ]

                else
                    []

        Nothing ->
            let
                firstOfMonth : Date.Date
                firstOfMonth =
                    Date.fromCalendarDate year month 1

                firstMatch : Date.Date
                firstMatch =
                    Date.ceiling interval firstOfMonth
            in
            allWeekdaysInMonth month firstMatch []


allWeekdaysInMonth : Time.Month -> Date.Date -> List Date.Date -> List Date.Date
allWeekdaysInMonth month current acc =
    if Date.month current /= month then
        List.reverse acc

    else
        allWeekdaysInMonth month (Date.add Date.Weeks 1 current) (current :: acc)


filterByMonth : List Time.Month -> List Date.Date -> List Date.Date
filterByMonth byMonth dates =
    if List.isEmpty byMonth then
        dates

    else
        List.filter (\d -> List.member (Date.month d) byMonth) dates


filterByMonthDay : List Int -> List Date.Date -> List Date.Date
filterByMonthDay byMonthDay dates =
    if List.isEmpty byMonthDay then
        dates

    else
        List.filter
            (\d ->
                let
                    maxDay : Int
                    maxDay =
                        daysInMonth (Date.year d) (Date.month d)

                    resolvedDays : List Int
                    resolvedDays =
                        List.filterMap
                            (\day ->
                                let
                                    actual : Int
                                    actual =
                                        if day < 0 then
                                            maxDay + day + 1

                                        else
                                            day
                                in
                                if actual >= 1 && actual <= maxDay then
                                    Just actual

                                else
                                    Nothing
                            )
                            byMonthDay
                in
                List.member (Date.day d) resolvedDays
            )
            dates


filterByDay : List DaySpec -> List Date.Date -> List Date.Date
filterByDay byDay dates =
    if List.isEmpty byDay then
        dates

    else
        let
            weekdays : List Time.Weekday
            weekdays =
                List.map .weekday byDay
        in
        List.filter (\d -> List.member (Date.weekday d) weekdays) dates


applyBySetPos : List Int -> List Date.Date -> List Date.Date
applyBySetPos bySetPos dates =
    if List.isEmpty bySetPos then
        dates

    else
        let
            len : Int
            len =
                List.length dates

            indexed : List ( Int, Date.Date )
            indexed =
                List.indexedMap Tuple.pair dates
        in
        bySetPos
            |> List.filterMap
                (\pos ->
                    let
                        idx : Int
                        idx =
                            if pos > 0 then
                                pos - 1

                            else
                                len + pos
                    in
                    indexed
                        |> List.filter (\( i, _ ) -> i == idx)
                        |> List.head
                        |> Maybe.map Tuple.second
                )
            |> List.sortBy Date.toRataDie


filterExclusions : Event -> List Date.Date -> List Date.Date
filterExclusions event dates =
    if List.isEmpty event.exclusions then
        dates

    else
        let
            exclusionRDs : List Int
            exclusionRDs =
                event.exclusions
                    |> List.map (\posix -> Date.toRataDie (Date.fromPosix Time.utc posix))
        in
        List.filter (\d -> not (List.member (Date.toRataDie d) exclusionRDs)) dates


occurrenceStartDate : EventTime -> Date.Date
occurrenceStartDate eventTime =
    case eventTime of
        AllDay { start } ->
            start

        WithTime { start } ->
            Date.fromPosix Time.utc start.posix

        FloatingTime { start } ->
            Date.fromCalendarDate start.year (Date.numberToMonth start.month) start.day


shiftTime : EventTime -> Date.Date -> Date.Date -> EventTime
shiftTime originalTime originalDate newDate =
    let
        dayDelta : Int
        dayDelta =
            Date.toRataDie newDate - Date.toRataDie originalDate
    in
    case originalTime of
        AllDay { start, end } ->
            AllDay
                { start = Date.add Date.Days dayDelta start
                , end = Maybe.map (Date.add Date.Days dayDelta) end
                }

        WithTime { start, end } ->
            let
                deltaMs : Int
                deltaMs =
                    dayDelta * 86400000
            in
            WithTime
                { start = { start | posix = Time.millisToPosix (Time.posixToMillis start.posix + deltaMs) }
                , end =
                    Maybe.map
                        (\e -> { e | posix = Time.millisToPosix (Time.posixToMillis e.posix + deltaMs) })
                        end
                }

        FloatingTime { start, end } ->
            FloatingTime
                { start = shiftLocalDateTime dayDelta start
                , end = Maybe.map (shiftLocalDateTime dayDelta) end
                }


shiftLocalDateTime : Int -> LocalDateTime -> LocalDateTime
shiftLocalDateTime dayDelta ldt =
    let
        originalDate : Date.Date
        originalDate =
            Date.fromCalendarDate ldt.year (Date.numberToMonth ldt.month) ldt.day

        newDate : Date.Date
        newDate =
            Date.add Date.Days dayDelta originalDate
    in
    { year = Date.year newDate
    , month = Date.monthToNumber (Date.month newDate)
    , day = Date.day newDate
    , hour = ldt.hour
    , minute = ldt.minute
    , second = ldt.second
    }


dedupOccurrences : List Occurrence -> List Occurrence
dedupOccurrences occurrences =
    dedupOccurrencesHelp occurrences -999999 []


dedupOccurrencesHelp : List Occurrence -> Int -> List Occurrence -> List Occurrence
dedupOccurrencesHelp remaining lastRD acc =
    case remaining of
        [] ->
            List.reverse acc

        occ :: rest ->
            let
                rd : Int
                rd =
                    Date.toRataDie (occurrenceStartDate occ.time)
            in
            if rd == lastRD then
                dedupOccurrencesHelp rest lastRD acc

            else
                dedupOccurrencesHelp rest rd (occ :: acc)


weekdayToInterval : Time.Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday

        Time.Sun ->
            Date.Sunday


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)
