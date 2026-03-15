module Ical.Parser exposing
    ( parse
    , Calendar, Event
    , EventTime(..), ResolvedTime, LocalDateTime, TimeZoneContext
    , Status(..), Transparency(..)
    , Organizer, RawProperty
    , Attendee, AttendeeRole(..), ParticipationStatus(..)
    , Alarm, AlarmAction(..), AlarmTrigger(..), AlarmTriggerRelative(..)
    , Journal, JournalTime(..), JournalStatus(..)
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

@docs EventTime, ResolvedTime, LocalDateTime, TimeZoneContext


## Enums and metadata

@docs Status, Transparency
@docs Organizer, RawProperty
@docs Attendee, AttendeeRole, ParticipationStatus


## Alarms

@docs Alarm, AlarmAction, AlarmTrigger, AlarmTriggerRelative


## Journals

@docs Journal, JournalTime, JournalStatus


## Recurrence expansion

@docs Occurrence, expand, expandNext

-}

import ContentLine exposing (ContentLine)
import Date
import DateHelpers
import Dict exposing (Dict)
import Ical.Recurrence as Recurrence exposing (DaySpec, Frequency(..), RecurrenceEnd(..), RecurrenceRule)
import Time
import VTimeZone
import ValueParser


{-| A parsed iCal calendar. Access fields directly:

    case Parser.parse icsString of
        Ok cal ->
            List.length cal.events

        Err err ->
            0

-}
type alias Calendar =
    { generatorProductId : String
    , specVersion : String
    , events : List Event
    , journals : List Journal
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
    , recurrenceDates : List Time.Posix
    , recurrenceId : Maybe Time.Posix
    , attendees : List Attendee
    , alarms : List Alarm
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


{-| A resolved date-time instant.

The `timeZoneName` is `Nothing` for UTC datetimes, or a
[IANA Time Zone Database](https://www.iana.org/time-zones) name
(e.g. `Just "America/New_York"`) for TZID-resolved datetimes.

For TZID values, `localDateTime` retains the original wall-clock datetime and
`timeZoneContext` keeps the parsed timezone definition so recurrence expansion
can stay in local calendar dates. Most consumers can ignore those fields and
just use `posix`.

-}
type alias ResolvedTime =
    { posix : Time.Posix
    , timeZoneName : Maybe String
    , localDateTime : Maybe LocalDateTime
    , timeZoneContext : Maybe TimeZoneContext
    }


{-| Opaque timezone data retained for parsed TZID date-times so recurrence
expansion can resolve future local wall-clock occurrences correctly.

Most code will only need [`ResolvedTime.posix`](#ResolvedTime) and
[`ResolvedTime.timeZoneName`](#ResolvedTime).

-}
type TimeZoneContext
    = TimeZoneContext VTimeZone.ZoneDefinition


{-| A date-time without timezone information. Represents a "floating" local
date-time that is interpreted in the viewer's local time.
-}
type alias LocalDateTime =
    { year : Int
    , month : Time.Month
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


{-| A parsed alarm (VALARM) attached to an event.

Each alarm has an `action` (`Display` or `Audio`), a `trigger` (when the alarm
fires), and an optional `description`.

    event.alarms
        |> List.map .trigger
        -- [ TriggerDuration (SecondsFromStart (-900)), ... ]

-}
type alias Alarm =
    { action : AlarmAction
    , trigger : AlarmTrigger
    , description : Maybe String
    }


{-| The type of alarm.
-}
type AlarmAction
    = Display
    | Audio


{-| When the alarm fires.

  - `TriggerDuration` — a relative offset from the event start or end.
  - `TriggerAbsolute` — a fixed UTC time (used with `VALUE=DATE-TIME`).

-}
type AlarmTrigger
    = TriggerDuration AlarmTriggerRelative
    | TriggerAbsolute Time.Posix


{-| A relative alarm trigger as a signed offset in seconds.
Negative values mean before, positive values mean after.

    -- 15 minutes before start
    SecondsFromStart (-15 * 60)

    -- at the moment the event ends
    SecondsFromEnd 0

-}
type AlarmTriggerRelative
    = SecondsFromStart Int
    | SecondsFromEnd Int


{-| A parsed iCal journal entry
([RFC 5545 Section 3.6.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.3)).
Unlike events, journals have no time span — just an optional date or datetime
stamp and text content. They represent dated notes or log entries.
-}
type alias Journal =
    { uid : String
    , stamp : Time.Posix
    , time : Maybe JournalTime
    , summary : Maybe String
    , description : Maybe String
    , status : Maybe JournalStatus
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , organizer : Maybe Organizer
    , extraProperties : List RawProperty
    }


{-| The time of a journal entry. Since journals have no time span (no DTEND
or DURATION), this is a single point — either a date or a datetime.
-}
type JournalTime
    = JournalDate Date.Date
    | JournalDateTime ResolvedTime
    | JournalFloatingTime LocalDateTime


{-| Journal status per
[RFC 5545 Section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11).

  - `Draft` — the journal entry is a draft.
  - `Final` — the journal entry is final.
  - `CancelledJournal` — the journal entry has been cancelled.
    (Named `CancelledJournal` to avoid a clash with [`Cancelled`](#Status).)

-}
type JournalStatus
    = Draft
    | Final
    | CancelledJournal


{-| Parse an iCal string into a Calendar.

    Ical.Parser.parse icsString
    --> Ok { generatorProductId = "-//My App//EN", specVersion = "2.0", events = [...], ... }

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
    { generatorProductId : Maybe String
    , specVersion : Maybe String
    , events : List Event
    , journals : List Journal
    , extraProperties : List RawProperty
    , timezones : Dict String VTimeZone.ZoneDefinition
    }


parseCalendar : List ContentLine -> Result String Calendar
parseCalendar contentLines =
    case contentLines of
        first :: rest ->
            if first.name == "BEGIN" && String.toUpper first.value == "VCALENDAR" then
                parseCalendarBody rest
                    { generatorProductId = Nothing
                    , specVersion = Nothing
                    , events = []
                    , journals = []
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
                case ( state.generatorProductId, state.specVersion ) of
                    ( Just generatorProductId, Just specVersion ) ->
                        Ok
                            { generatorProductId = generatorProductId
                            , specVersion = specVersion
                            , events = List.reverse state.events
                            , journals = List.reverse state.journals
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

            else if line.name == "BEGIN" && String.toUpper line.value == "VJOURNAL" then
                case parseJournal rest state.timezones emptyJournalAccum of
                    Ok ( journal, remaining ) ->
                        parseCalendarBody remaining { state | journals = journal :: state.journals }

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
                        parseCalendarBody rest { state | specVersion = Just line.value }

                    "PRODID" ->
                        parseCalendarBody rest { state | generatorProductId = Just line.value }

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
    , recurrenceDates : List Time.Posix
    , recurrenceId : Maybe Time.Posix
    , attendees : List Attendee
    , alarms : List Alarm
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
    , recurrenceDates = []
    , recurrenceId = Nothing
    , attendees = []
    , alarms = []
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
                                                                    , recurrenceDates = List.reverse accum.recurrenceDates
                                                                    , recurrenceId = accum.recurrenceId
                                                                    , attendees = List.reverse accum.attendees
                                                                    , alarms = List.reverse accum.alarms
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
                                    if durationHasTimeParts dur then
                                        Err "DATE DTSTART must use a day- or week-based DURATION"

                                    else
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


durationHasTimeParts : ValueParser.Duration -> Bool
durationHasTimeParts duration =
    duration.hours /= 0 || duration.minutes /= 0 || duration.seconds /= 0


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
    case ( resolved.timeZoneName, resolved.localDateTime, resolved.timeZoneContext ) of
        ( Just timeZoneName, Just localDateTime, Just timeZoneContext ) ->
            resolveTimeZoneLocalDateTime timeZoneName timeZoneContext (addDurationToLocal dur localDateTime)
                |> Maybe.withDefault (addDurationToResolvedUtc dur resolved)

        _ ->
            addDurationToResolvedUtc dur resolved


addDurationToResolvedUtc : ValueParser.Duration -> ResolvedTime -> ResolvedTime
addDurationToResolvedUtc dur resolved =
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
    , localDateTime = Nothing
    , timeZoneContext = resolved.timeZoneContext
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
            Date.fromCalendarDate dt.year dt.month dt.day

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
    , month = Date.month newDate
    , day = Date.day newDate
    , hour = remainingSeconds // 3600
    , minute = modBy 60 (remainingSeconds // 60)
    , second = modBy 60 remainingSeconds
    }


resolveTimeZoneLocalDateTime : String -> TimeZoneContext -> LocalDateTime -> Maybe ResolvedTime
resolveTimeZoneLocalDateTime timeZoneName timeZoneContext localDateTime =
    let
        zoneDefinition : VTimeZone.ZoneDefinition
        zoneDefinition =
            timeZoneDefinitionFromContext timeZoneContext
    in
    VTimeZone.resolve zoneDefinition localDateTime
        |> Result.toMaybe
        |> Maybe.map
            (\posix ->
                { posix = posix
                , timeZoneName = Just timeZoneName
                , localDateTime = Just localDateTime
                , timeZoneContext = Just timeZoneContext
                }
            )


resolvedTimeFromPosixInTimeZone : String -> TimeZoneContext -> Time.Posix -> ResolvedTime
resolvedTimeFromPosixInTimeZone timeZoneName timeZoneContext posix =
    let
        zoneDefinition : VTimeZone.ZoneDefinition
        zoneDefinition =
            timeZoneDefinitionFromContext timeZoneContext
    in
    { posix = posix
    , timeZoneName = Just timeZoneName
    , localDateTime = VTimeZone.toLocal zoneDefinition posix |> Result.toMaybe
    , timeZoneContext = Just timeZoneContext
    }


timeZoneDefinitionFromContext : TimeZoneContext -> VTimeZone.ZoneDefinition
timeZoneDefinitionFromContext (TimeZoneContext zoneDefinition) =
    zoneDefinition


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

            else if line.name == "BEGIN" && String.toUpper line.value == "VALARM" then
                case parseAlarm rest of
                    Ok ( alarm, remaining ) ->
                        parseEvent remaining timezones { accum | alarms = alarm :: accum.alarms }

                    Err err ->
                        Err err

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
            case parseExdateValues timezones line of
                Ok values ->
                    { accum | exclusions = List.reverse values ++ accum.exclusions }

                Err err ->
                    addEventError ("Invalid EXDATE: " ++ err) accum

        "RDATE" ->
            case parseExdateValues timezones line of
                Ok values ->
                    { accum | recurrenceDates = List.reverse values ++ accum.recurrenceDates }

                Err err ->
                    addEventError ("Invalid RDATE: " ++ err) accum

        "RECURRENCE-ID" ->
            case parseDateTimeValue timezones line of
                Ok (IDate date) ->
                    { accum | recurrenceId = Just (dateToUtcMidnight date) }

                Ok (IDateTime { posix }) ->
                    { accum | recurrenceId = Just posix }

                Ok (IFloating localDateTime) ->
                    { accum | recurrenceId = Just (dateToUtcMidnight (Date.fromCalendarDate localDateTime.year localDateTime.month localDateTime.day)) }

                Err err ->
                    addEventError ("Invalid RECURRENCE-ID: " ++ err) accum

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
        case tzid of
            Just _ ->
                Err "TZID must not be applied to DATE values"

            Nothing ->
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
            , month = Date.numberToMonth parts.month
            , day = parts.day
            , hour = parts.hour
            , minute = parts.minute
            , second = parts.second
            }
    in
    case maybeTzid of
        Just tz ->
            if parts.isUtc then
                Err "TZID must not be applied to UTC DATE-TIME values"

            else
                case Dict.get tz timezones of
                    Just zoneDef ->
                        VTimeZone.resolve zoneDef dt
                            |> Result.map
                                (\posix ->
                                    IDateTime
                                        { posix = posix
                                        , timeZoneName = Just tz
                                        , localDateTime = Just dt
                                        , timeZoneContext = Just (TimeZoneContext zoneDef)
                                        }
                                )

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
                Ok
                    (IDateTime
                        { posix = Time.millisToPosix (totalSeconds * 1000)
                        , timeZoneName = Nothing
                        , localDateTime = Nothing
                        , timeZoneContext = Nothing
                        }
                    )

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


parseExdateValues : Dict String VTimeZone.ZoneDefinition -> ContentLine -> Result String (List Time.Posix)
parseExdateValues timezones line =
    String.split "," line.value
        |> List.foldr
            (\val resultSoFar ->
                let
                    fakeLine : ContentLine
                    fakeLine =
                        { name = "EXDATE"
                        , parameters = line.parameters
                        , value = val
                        }
                in
                case ( parseDateTimeValue timezones fakeLine, resultSoFar ) of
                    ( Ok (IDate date), Ok soFar ) ->
                        Ok (dateToUtcMidnight date :: soFar)

                    ( Ok (IDateTime { posix }), Ok soFar ) ->
                        Ok (posix :: soFar)

                    ( Ok (IFloating localDateTime), Ok soFar ) ->
                        Ok
                            (dateToUtcMidnight
                                (Date.fromCalendarDate localDateTime.year localDateTime.month localDateTime.day)
                                :: soFar
                            )

                    ( Err err, _ ) ->
                        Err ("could not parse value \"" ++ val ++ "\": " ++ err)

                    ( _, Err err ) ->
                        Err err
            )
            (Ok [])


dateToUtcMidnight : Date.Date -> Time.Posix
dateToUtcMidnight date =
    Time.millisToPosix ((Date.toRataDie date - 719163) * 86400 * 1000)



-- VJOURNAL PARSING


type alias JournalAccum =
    { uid : Maybe String
    , dtstamp : Maybe InternalDateTimeValue
    , dtstart : Maybe InternalDateTimeValue
    , created : Maybe InternalDateTimeValue
    , lastModified : Maybe InternalDateTimeValue
    , summary : Maybe String
    , description : Maybe String
    , organizer : Maybe Organizer
    , status : Maybe JournalStatus
    , extraProperties : List RawProperty
    , errors : List String
    }


emptyJournalAccum : JournalAccum
emptyJournalAccum =
    { uid = Nothing
    , dtstamp = Nothing
    , dtstart = Nothing
    , created = Nothing
    , lastModified = Nothing
    , summary = Nothing
    , description = Nothing
    , organizer = Nothing
    , status = Nothing
    , extraProperties = []
    , errors = []
    }


parseJournal : List ContentLine -> Dict String VTimeZone.ZoneDefinition -> JournalAccum -> Result String ( Journal, List ContentLine )
parseJournal lines timezones accum =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VJOURNAL"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VJOURNAL" then
                finalizeJournal accum
                    |> Result.map (\journal -> ( journal, rest ))

            else if line.name == "END" then
                Err ("Mismatched END: expected VJOURNAL, got " ++ line.value)

            else if line.name == "BEGIN" then
                case skipComponent (String.toUpper line.value) rest of
                    Ok remaining ->
                        parseJournal remaining timezones accum

                    Err err ->
                        Err err

            else
                parseJournal rest timezones (applyJournalProperty timezones line accum)


applyJournalProperty : Dict String VTimeZone.ZoneDefinition -> ContentLine -> JournalAccum -> JournalAccum
applyJournalProperty timezones line accum =
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
                    addJournalError ("Invalid DTSTAMP: " ++ err) accum

        "DTSTART" ->
            case parseDateTimeValue timezones line of
                Ok dtstart ->
                    { accum | dtstart = Just dtstart }

                Err err ->
                    addJournalError ("Invalid DTSTART: " ++ err) accum

        "CREATED" ->
            case parseDateTimeValue timezones line of
                Ok created ->
                    { accum | created = Just created }

                Err err ->
                    addJournalError ("Invalid CREATED: " ++ err) accum

        "LAST-MODIFIED" ->
            case parseDateTimeValue timezones line of
                Ok lastModified ->
                    { accum | lastModified = Just lastModified }

                Err err ->
                    addJournalError ("Invalid LAST-MODIFIED: " ++ err) accum

        "SUMMARY" ->
            { accum | summary = Just (ValueParser.unescapeText line.value) }

        "DESCRIPTION" ->
            { accum | description = Just (ValueParser.unescapeText line.value) }

        "ORGANIZER" ->
            { accum | organizer = Just (parseOrganizer line) }

        "STATUS" ->
            case parseJournalStatus line.value of
                Just s ->
                    { accum | status = Just s }

                Nothing ->
                    { accum | extraProperties = rawProp :: accum.extraProperties }

        _ ->
            { accum | extraProperties = rawProp :: accum.extraProperties }


addJournalError : String -> JournalAccum -> JournalAccum
addJournalError error accum =
    { accum | errors = error :: accum.errors }


parseJournalStatus : String -> Maybe JournalStatus
parseJournalStatus value =
    case String.toUpper value of
        "DRAFT" ->
            Just Draft

        "FINAL" ->
            Just Final

        "CANCELLED" ->
            Just CancelledJournal

        _ ->
            Nothing


finalizeJournal : JournalAccum -> Result String Journal
finalizeJournal accum =
    case List.reverse accum.errors of
        firstError :: _ ->
            Err firstError

        [] ->
            case ( accum.uid, accum.dtstamp ) of
                ( Just uid, Just dtstamp ) ->
                    extractPosix "DTSTAMP" dtstamp
                        |> Result.andThen
                            (\stamp ->
                                let
                                    journalTime : Maybe JournalTime
                                    journalTime =
                                        accum.dtstart
                                            |> Maybe.map
                                                (\dtstart ->
                                                    case dtstart of
                                                        IDate date ->
                                                            JournalDate date

                                                        IDateTime resolved ->
                                                            JournalDateTime resolved

                                                        IFloating local ->
                                                            JournalFloatingTime local
                                                )
                                in
                                extractMaybePosix "CREATED" accum.created
                                    |> Result.andThen
                                        (\created ->
                                            extractMaybePosix "LAST-MODIFIED" accum.lastModified
                                                |> Result.map
                                                    (\lastModified ->
                                                        { uid = uid
                                                        , stamp = stamp
                                                        , time = journalTime
                                                        , summary = accum.summary
                                                        , description = accum.description
                                                        , status = accum.status
                                                        , created = created
                                                        , lastModified = lastModified
                                                        , organizer = accum.organizer
                                                        , extraProperties = List.reverse accum.extraProperties
                                                        }
                                                    )
                                        )
                            )

                ( Nothing, _ ) ->
                    Err "VJOURNAL missing required UID"

                ( _, Nothing ) ->
                    Err "VJOURNAL missing required DTSTAMP"


type alias AlarmAccum =
    { action : Maybe AlarmAction
    , trigger : Maybe AlarmTrigger
    , description : Maybe String
    , errors : List String
    }


parseAlarm : List ContentLine -> Result String ( Alarm, List ContentLine )
parseAlarm lines =
    parseAlarmBody lines { action = Nothing, trigger = Nothing, description = Nothing, errors = [] }


parseAlarmBody : List ContentLine -> AlarmAccum -> Result String ( Alarm, List ContentLine )
parseAlarmBody lines accum =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VALARM"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VALARM" then
                case List.reverse accum.errors of
                    firstError :: _ ->
                        Err firstError

                    [] ->
                        case ( accum.action, accum.trigger ) of
                            ( Just action, Just trigger ) ->
                                Ok
                                    ( { action = action
                                      , trigger = trigger
                                      , description = accum.description
                                      }
                                    , rest
                                    )

                            ( Nothing, _ ) ->
                                Err "VALARM missing required ACTION"

                            ( _, Nothing ) ->
                                Err "VALARM missing required TRIGGER"

            else
                parseAlarmBody rest (applyAlarmProperty line accum)


applyAlarmProperty : ContentLine -> AlarmAccum -> AlarmAccum
applyAlarmProperty line accum =
    case String.toUpper line.name of
        "ACTION" ->
            case String.toUpper line.value of
                "DISPLAY" ->
                    { accum | action = Just Display }

                "AUDIO" ->
                    { accum | action = Just Audio }

                _ ->
                    { accum | errors = ("Invalid VALARM ACTION: " ++ line.value) :: accum.errors }

        "TRIGGER" ->
            case parseTrigger line of
                Ok trigger ->
                    { accum | trigger = Just trigger }

                Err err ->
                    { accum | errors = ("Invalid VALARM TRIGGER: " ++ err) :: accum.errors }

        "DESCRIPTION" ->
            { accum | description = Just (ValueParser.unescapeText line.value) }

        _ ->
            accum


parseTrigger : ContentLine -> Result String AlarmTrigger
parseTrigger line =
    let
        hasValueDateTime : Bool
        hasValueDateTime =
            line.parameters
                |> List.any
                    (\( k, v ) ->
                        String.toUpper k == "VALUE" && String.toUpper v == "DATE-TIME"
                    )

        maybeRelated : Maybe String
        maybeRelated =
            line.parameters
                |> List.filterMap
                    (\( k, v ) ->
                        if String.toUpper k == "RELATED" then
                            Just (String.toUpper v)

                        else
                            Nothing
                    )
                |> List.head
    in
    if hasValueDateTime then
        ValueParser.parseDateTime line.value
            |> Result.andThen
                (\parts ->
                    if maybeRelated /= Nothing then
                        Err "RELATED is only valid for DURATION triggers"

                    else if not parts.isUtc then
                        Err "Absolute DATE-TIME triggers must be UTC"

                    else
                        let
                            date : Date.Date
                            date =
                                Date.fromCalendarDate parts.year (Date.numberToMonth parts.month) parts.day

                            millis : Int
                            millis =
                                dateToUnixMs date
                                    + parts.hour
                                    * 3600000
                                    + parts.minute
                                    * 60000
                                    + parts.second
                                    * 1000
                        in
                        Ok (TriggerAbsolute (Time.millisToPosix millis))
                )

    else
        parseSignedDuration line.value
            |> Result.map
                (\totalSeconds ->
                    if maybeRelated == Just "END" then
                        TriggerDuration (SecondsFromEnd totalSeconds)

                    else
                        TriggerDuration (SecondsFromStart totalSeconds)
                )


parseSignedDuration : String -> Result String Int
parseSignedDuration input =
    let
        ( isNegative, stripped ) =
            if String.startsWith "-" input then
                ( True, String.dropLeft 1 input )

            else if String.startsWith "+" input then
                ( False, String.dropLeft 1 input )

            else
                ( False, input )
    in
    ValueParser.parseDuration stripped
        |> Result.map
            (\dur ->
                let
                    totalSeconds : Int
                    totalSeconds =
                        dur.weeks
                            * 604800
                            + dur.days
                            * 86400
                            + dur.hours
                            * 3600
                            + dur.minutes
                            * 60
                            + dur.seconds
                in
                if isNegative then
                    -totalSeconds

                else
                    totalSeconds
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


{-| Expand events into concrete occurrences within a date range, merging
RECURRENCE-ID overrides automatically. Override events (those with a
`recurrenceId`) replace the matching occurrence from the master event that
shares their `uid`.

    case Parser.parse icsString of
        Ok cal ->
            Parser.expand { start = jan1, end = dec31 } cal.events

        Err err ->
            []

For a single event, wrap it in a list:

    Parser.expand { start = jan1, end = dec31 } [ event ]

-}
expand : { start : Date.Date, end : Date.Date } -> List Event -> List Occurrence
expand range events =
    let
        ( masterEvents, overrideEvents ) =
            List.partition (\e -> e.recurrenceId == Nothing) events

        masterOccurrences : List Occurrence
        masterOccurrences =
            masterEvents
                |> List.concatMap (expandEvent range)

        overrideOccurrences : List Occurrence
        overrideOccurrences =
            overrideEvents
                |> List.concatMap (expandEvent range)

        overriddenKeys : List ( String, Int )
        overriddenKeys =
            overrideEvents
                |> List.filterMap
                    (\e ->
                        e.recurrenceId
                            |> Maybe.map
                                (\posix -> ( e.uid, recurrenceReferenceKey e.time posix ))
                    )

        isOverridden : Occurrence -> Bool
        isOverridden occ =
            List.member
                ( occ.event.uid, occurrenceReferenceKeyFromOccurrence occ )
                overriddenKeys
    in
    (List.filter (\occ -> not (isOverridden occ)) masterOccurrences ++ overrideOccurrences)
        |> List.sortBy (\occ -> occurrenceTimeKey occ.time)


expandEvent : { start : Date.Date, end : Date.Date } -> Event -> List Occurrence
expandEvent range event =
    let
        baseOccurrences : List Occurrence
        baseOccurrences =
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

        rdateOccurrences : List Occurrence
        rdateOccurrences =
            event.recurrenceDates
                |> List.map (occurrenceFromRecurrenceDate event)
                |> List.filter
                    (\occ ->
                        let
                            startDate : Date.Date
                            startDate =
                                occurrenceStartDate occ.time
                        in
                        Date.compare startDate range.start /= LT && Date.compare startDate range.end /= GT
                    )
    in
    (baseOccurrences ++ rdateOccurrences)
        |> List.filter (not << occurrenceIsExcluded event)
        |> List.sortBy (\occ -> occurrenceTimeKey occ.time)
        |> dedupOccurrences


{-| Get the next N occurrences starting from a given date, merging
RECURRENCE-ID overrides automatically.

    case Parser.parse icsString of
        Ok cal ->
            Parser.expandNext 10 today cal.events

        Err err ->
            []

For a single event, wrap it in a list:

    Parser.expandNext 5 today [ event ]

-}
expandNext : Int -> Date.Date -> List Event -> List Occurrence
expandNext n fromDate events =
    let
        ( masterEvents, overrideEvents ) =
            List.partition (\e -> e.recurrenceId == Nothing) events

        masterOccurrences : List Occurrence
        masterOccurrences =
            masterEvents
                |> List.concatMap (expandNextEvent n fromDate)

        overrideOccurrences : List Occurrence
        overrideOccurrences =
            overrideEvents
                |> List.concatMap (expandNextEvent n fromDate)

        overriddenKeys : List ( String, Int )
        overriddenKeys =
            overrideEvents
                |> List.filterMap
                    (\e ->
                        e.recurrenceId
                            |> Maybe.map
                                (\posix -> ( e.uid, recurrenceReferenceKey e.time posix ))
                    )

        isOverridden : Occurrence -> Bool
        isOverridden occ =
            List.member
                ( occ.event.uid, occurrenceReferenceKeyFromOccurrence occ )
                overriddenKeys
    in
    (List.filter (\occ -> not (isOverridden occ)) masterOccurrences ++ overrideOccurrences)
        |> List.sortBy (\occ -> occurrenceTimeKey occ.time)
        |> List.take n


expandNextEvent : Int -> Date.Date -> Event -> List Occurrence
expandNextEvent n fromDate event =
    let
        baseOccurrences : List Occurrence
        baseOccurrences =
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
                        |> List.concatMap (\rule -> expandNextRule (n + List.length event.exclusions) fromDate event rule)

        rdateOccurrences : List Occurrence
        rdateOccurrences =
            event.recurrenceDates
                |> List.map (occurrenceFromRecurrenceDate event)
                |> List.filter (\occ -> Date.compare (occurrenceStartDate occ.time) fromDate /= LT)
    in
    (baseOccurrences ++ rdateOccurrences)
        |> List.filter (not << occurrenceIsExcluded event)
        |> List.sortBy (\occ -> occurrenceTimeKey occ.time)
        |> dedupOccurrences
        |> List.take n


expandNextRule : Int -> Date.Date -> Event -> RecurrenceRule -> List Occurrence
expandNextRule n fromDate event rule =
    expandNextChunked n fromDate event rule 1 []


expandNextChunked : Int -> Date.Date -> Event -> RecurrenceRule -> Int -> List Occurrence -> List Occurrence
expandNextChunked needed fromDate event rule chunkYears acc =
    let
        range : { start : Date.Date, end : Date.Date }
        range =
            { start = fromDate
            , end = Date.add Date.Days (chunkYears * 366) fromDate
            }

        newOccurrences : List Occurrence
        newOccurrences =
            expandRule range event rule

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
                    expandNextChunked needed fromDate event rule (chunkYears * 2) []

            _ ->
                combined


expandRule : { start : Date.Date, end : Date.Date } -> Event -> RecurrenceRule -> List Occurrence
expandRule range event rule =
    case frequencyToMillis rule.frequency of
        Just intervalMs ->
            expandSubDaily range event rule intervalMs

        Nothing ->
            let
                hasTimeParts : Bool
                hasTimeParts =
                    not (List.isEmpty rule.byHour && List.isEmpty rule.byMinute && List.isEmpty rule.bySecond)

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

                occurrences : List Occurrence
                occurrences =
                    candidates
                        |> List.filter (\d -> Date.toRataDie d >= rangeStartRD && Date.toRataDie d <= rangeEndRD)
                        |> List.concatMap
                            (\d ->
                                expandByTimeParts rule event seed d
                            )
            in
            -- When time parts expand each date into multiple occurrences,
            -- COUNT should limit the total occurrence count, not the date count.
            if hasTimeParts then
                case rule.end of
                    Count n ->
                        List.take n occurrences

                    _ ->
                        occurrences

            else
                occurrences


frequencyToMillis : Frequency -> Maybe Int
frequencyToMillis freq =
    case freq of
        Secondly { every } ->
            Just (every * 1000)

        Minutely { every } ->
            Just (every * 60 * 1000)

        Hourly { every } ->
            Just (every * 3600 * 1000)

        _ ->
            Nothing


expandSubDaily : { start : Date.Date, end : Date.Date } -> Event -> RecurrenceRule -> Int -> List Occurrence
expandSubDaily range event rule intervalMs =
    case event.time of
        WithTime { start, end } ->
            let
                startMs : Int
                startMs =
                    Time.posixToMillis start.posix

                durationMs : Int
                durationMs =
                    case end of
                        Just e ->
                            Time.posixToMillis e.posix - startMs

                        Nothing ->
                            0

                rangeStartMs : Int
                rangeStartMs =
                    dateToUnixMs range.start

                rangeEndMs : Int
                rangeEndMs =
                    dateToUnixMs range.end + 86400000

                exclusionMs : List Int
                exclusionMs =
                    event.exclusions |> List.map Time.posixToMillis
            in
            subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event start.timeZoneName 0 0 []

        _ ->
            -- Sub-daily frequencies don't apply to all-day or floating events
            []


subDailyLoop :
    RecurrenceRule
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List Int
    -> Event
    -> Maybe String
    -> Int
    -> Int
    -> List Occurrence
    -> List Occurrence
subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName stepIndex emittedCount acc =
    let
        currentMs : Int
        currentMs =
            startMs + stepIndex * intervalMs

        pastEnd : Bool
        pastEnd =
            currentMs >= rangeEndMs

        excluded : Bool
        excluded =
            List.member currentMs exclusionMs

        filtered : Bool
        filtered =
            not (passesSubDailyFilters rule currentMs)

        inRange : Bool
        inRange =
            currentMs >= rangeStartMs
    in
    if pastEnd && not (isCountLimited rule.end) then
        List.reverse acc

    else
        case rule.end of
            Count n ->
                if emittedCount >= n || pastEnd then
                    List.reverse acc

                else if excluded || filtered then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

                else if inRange then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) (emittedCount + 1) (makeSubDailyOccurrence event tzName currentMs durationMs :: acc)

                else
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

            UntilDateTime untilPosix ->
                if currentMs > Time.posixToMillis untilPosix then
                    List.reverse acc

                else if excluded || filtered then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

                else if inRange then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) (emittedCount + 1) (makeSubDailyOccurrence event tzName currentMs durationMs :: acc)

                else
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

            UntilDate untilDate ->
                if Date.toRataDie (Date.fromPosix Time.utc (Time.millisToPosix currentMs)) > Date.toRataDie untilDate then
                    List.reverse acc

                else if excluded || filtered then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

                else if inRange then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) (emittedCount + 1) (makeSubDailyOccurrence event tzName currentMs durationMs :: acc)

                else
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

            Forever ->
                if pastEnd then
                    List.reverse acc

                else if excluded || filtered then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc

                else if inRange then
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) (emittedCount + 1) (makeSubDailyOccurrence event tzName currentMs durationMs :: acc)

                else
                    subDailyLoop rule startMs intervalMs durationMs rangeStartMs rangeEndMs exclusionMs event tzName (stepIndex + 1) emittedCount acc


passesSubDailyFilters : RecurrenceRule -> Int -> Bool
passesSubDailyFilters rule currentMs =
    let
        posix : Time.Posix
        posix =
            Time.millisToPosix currentMs

        date : Date.Date
        date =
            Date.fromPosix Time.utc posix
    in
    (List.isEmpty rule.byMonth || List.member (Date.month date) rule.byMonth)
        && (List.isEmpty rule.byMonthDay || List.member (Date.day date) rule.byMonthDay)
        && (List.isEmpty rule.byDay || List.member (Date.weekday date) (List.map daySpecWeekday rule.byDay))
        && (List.isEmpty rule.byHour || List.member (Time.toHour Time.utc posix) rule.byHour)
        && (List.isEmpty rule.byMinute || List.member (Time.toMinute Time.utc posix) rule.byMinute)
        && (List.isEmpty rule.bySecond || List.member (Time.toSecond Time.utc posix) rule.bySecond)
        && (List.isEmpty rule.byYearDay || List.member (Date.ordinalDay date) rule.byYearDay)
        && (List.isEmpty rule.byWeekNo || List.member (Date.weekNumber date) rule.byWeekNo)


makeSubDailyOccurrence : Event -> Maybe String -> Int -> Int -> Occurrence
makeSubDailyOccurrence event tzName currentMs durationMs =
    let
        startPosix : Time.Posix
        startPosix =
            Time.millisToPosix currentMs

        endPosix : Time.Posix
        endPosix =
            Time.millisToPosix (currentMs + durationMs)
    in
    { event = event
    , time =
        WithTime
            { start =
                { posix = startPosix
                , timeZoneName = tzName
                , localDateTime = Nothing
                , timeZoneContext = Nothing
                }
            , end =
                Just
                    { posix = endPosix
                    , timeZoneName = tzName
                    , localDateTime = Nothing
                    , timeZoneContext = Nothing
                    }
            }
    }


expandByTimeParts : RecurrenceRule -> Event -> Date.Date -> Date.Date -> List Occurrence
expandByTimeParts rule event seed candidateDate =
    if List.isEmpty rule.byHour && List.isEmpty rule.byMinute && List.isEmpty rule.bySecond then
        [ { event = event, time = shiftTime event.time seed candidateDate } ]

    else
        case event.time of
            WithTime { start, end } ->
                case ( start.timeZoneName, start.localDateTime, start.timeZoneContext ) of
                    ( Just timeZoneName, Just startLocalDateTime, Just timeZoneContext ) ->
                        let
                            durationMs : Int
                            durationMs =
                                case end of
                                    Just e ->
                                        Time.posixToMillis e.posix - Time.posixToMillis start.posix

                                    Nothing ->
                                        0

                            hours : List Int
                            hours =
                                if List.isEmpty rule.byHour then
                                    [ startLocalDateTime.hour ]

                                else
                                    rule.byHour

                            minutes : List Int
                            minutes =
                                if List.isEmpty rule.byMinute then
                                    [ startLocalDateTime.minute ]

                                else
                                    rule.byMinute

                            seconds : List Int
                            seconds =
                                if List.isEmpty rule.bySecond then
                                    [ startLocalDateTime.second ]

                                else
                                    rule.bySecond
                        in
                        hours
                            |> List.concatMap
                                (\h ->
                                    minutes
                                        |> List.concatMap
                                            (\m ->
                                                seconds
                                                    |> List.filterMap
                                                        (\s ->
                                                            let
                                                                candidateLocalDateTime : LocalDateTime
                                                                candidateLocalDateTime =
                                                                    { year = Date.year candidateDate
                                                                    , month = Date.month candidateDate
                                                                    , day = Date.day candidateDate
                                                                    , hour = h
                                                                    , minute = m
                                                                    , second = s
                                                                    }
                                                            in
                                                            resolveTimeZoneLocalDateTime timeZoneName timeZoneContext candidateLocalDateTime
                                                                |> Maybe.map
                                                                    (\resolvedStart ->
                                                                        { event = event
                                                                        , time =
                                                                            WithTime
                                                                                { start = resolvedStart
                                                                                , end =
                                                                                    Maybe.map
                                                                                        (\_ ->
                                                                                            resolvedTimeFromPosixInTimeZone
                                                                                                timeZoneName
                                                                                                timeZoneContext
                                                                                                (Time.millisToPosix (Time.posixToMillis resolvedStart.posix + durationMs))
                                                                                        )
                                                                                        end
                                                                                }
                                                                        }
                                                                    )
                                                        )
                                            )
                                )

                    _ ->
                        let
                            dayDelta : Int
                            dayDelta =
                                Date.toRataDie candidateDate - Date.toRataDie seed

                            durationMs : Int
                            durationMs =
                                case end of
                                    Just e ->
                                        Time.posixToMillis e.posix - Time.posixToMillis start.posix

                                    Nothing ->
                                        0

                            origHour : Int
                            origHour =
                                Time.toHour Time.utc start.posix

                            origMinute : Int
                            origMinute =
                                Time.toMinute Time.utc start.posix

                            origSecond : Int
                            origSecond =
                                Time.toSecond Time.utc start.posix

                            hours : List Int
                            hours =
                                if List.isEmpty rule.byHour then
                                    [ origHour ]

                                else
                                    rule.byHour

                            minutes : List Int
                            minutes =
                                if List.isEmpty rule.byMinute then
                                    [ origMinute ]

                                else
                                    rule.byMinute

                            seconds : List Int
                            seconds =
                                if List.isEmpty rule.bySecond then
                                    [ origSecond ]

                                else
                                    rule.bySecond

                            -- Midnight of the candidate date in ms
                            baseDayMs : Int
                            baseDayMs =
                                Time.posixToMillis start.posix + dayDelta * 86400000 - (origHour * 3600000 + origMinute * 60000 + origSecond * 1000)
                        in
                        hours
                            |> List.concatMap
                                (\h ->
                                    minutes
                                        |> List.concatMap
                                            (\m ->
                                                seconds
                                                    |> List.map
                                                        (\s ->
                                                            let
                                                                newStartMs : Int
                                                                newStartMs =
                                                                    baseDayMs + h * 3600000 + m * 60000 + s * 1000
                                                            in
                                                            { event = event
                                                            , time =
                                                                WithTime
                                                                    { start =
                                                                        { posix = Time.millisToPosix newStartMs
                                                                        , timeZoneName = start.timeZoneName
                                                                        , localDateTime = Nothing
                                                                        , timeZoneContext = start.timeZoneContext
                                                                        }
                                                                    , end =
                                                                        Just
                                                                            { posix = Time.millisToPosix (newStartMs + durationMs)
                                                                            , timeZoneName = start.timeZoneName
                                                                            , localDateTime = Nothing
                                                                            , timeZoneContext = start.timeZoneContext
                                                                            }
                                                                    }
                                                            }
                                                        )
                                            )
                                )

            FloatingTime { start, end } ->
                let
                    dayDelta : Int
                    dayDelta =
                        Date.toRataDie candidateDate - Date.toRataDie seed

                    origDuration : { hours : Int, minutes : Int, seconds : Int }
                    origDuration =
                        case end of
                            Just e ->
                                let
                                    startSecs : Int
                                    startSecs =
                                        start.hour * 3600 + start.minute * 60 + start.second

                                    endSecs : Int
                                    endSecs =
                                        e.hour * 3600 + e.minute * 60 + e.second

                                    diff : Int
                                    diff =
                                        endSecs - startSecs
                                in
                                { hours = diff // 3600, minutes = modBy 60 (diff // 60), seconds = modBy 60 diff }

                            Nothing ->
                                { hours = 0, minutes = 0, seconds = 0 }

                    hours : List Int
                    hours =
                        if List.isEmpty rule.byHour then
                            [ start.hour ]

                        else
                            rule.byHour

                    minutes : List Int
                    minutes =
                        if List.isEmpty rule.byMinute then
                            [ start.minute ]

                        else
                            rule.byMinute

                    seconds : List Int
                    seconds =
                        if List.isEmpty rule.bySecond then
                            [ start.second ]

                        else
                            rule.bySecond

                    newDate : Date.Date
                    newDate =
                        Date.add Date.Days dayDelta (Date.fromCalendarDate start.year start.month start.day)
                in
                hours
                    |> List.concatMap
                        (\h ->
                            minutes
                                |> List.concatMap
                                    (\mn ->
                                        seconds
                                            |> List.map
                                                (\s ->
                                                    let
                                                        newStart : LocalDateTime
                                                        newStart =
                                                            { year = Date.year newDate
                                                            , month = Date.month newDate
                                                            , day = Date.day newDate
                                                            , hour = h
                                                            , minute = mn
                                                            , second = s
                                                            }

                                                        newEnd : LocalDateTime
                                                        newEnd =
                                                            { year = Date.year newDate
                                                            , month = Date.month newDate
                                                            , day = Date.day newDate
                                                            , hour = h + origDuration.hours
                                                            , minute = mn + origDuration.minutes
                                                            , second = s + origDuration.seconds
                                                            }
                                                    in
                                                    { event = event
                                                    , time =
                                                        FloatingTime
                                                            { start = newStart
                                                            , end = Just newEnd
                                                            }
                                                    }
                                                )
                                    )
                        )

            AllDay _ ->
                [ { event = event, time = shiftTime event.time seed candidateDate } ]


generateCandidates : RecurrenceRule -> EventTime -> Date.Date -> Int -> List Date.Date
generateCandidates rule originalTime seed rangeEndRD =
    generateLoop rule originalTime seed rangeEndRD 0 0 []


generateLoop : RecurrenceRule -> EventTime -> Date.Date -> Int -> Int -> Int -> List Date.Date -> List Date.Date
generateLoop rule originalTime seed rangeEndRD stepIndex emittedCount acc =
    let
        intervalDate : Date.Date
        intervalDate =
            advanceByInterval rule.frequency (frequencyEvery rule.frequency * stepIndex) seed

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
                (Date.fromCalendarDate start.year start.month start.day)
                (Date.fromPosix Time.utc untilPosix)
                == GT


isCountLimited : RecurrenceEnd -> Bool
isCountLimited end =
    case end of
        Count _ ->
            True

        _ ->
            False


frequencyEvery : Frequency -> Int
frequencyEvery freq =
    case freq of
        Secondly { every } ->
            every

        Minutely { every } ->
            every

        Hourly { every } ->
            every

        Daily { every } ->
            every

        Weekly { every } ->
            every

        Monthly { every } ->
            every

        Yearly { every } ->
            every


advanceByInterval : Frequency -> Int -> Date.Date -> Date.Date
advanceByInterval freq n seed =
    case freq of
        Secondly _ ->
            -- Sub-daily frequencies don't advance by date; same date each step
            seed

        Minutely _ ->
            seed

        Hourly _ ->
            seed

        Daily _ ->
            Date.add Date.Days n seed

        Weekly _ ->
            Date.add Date.Weeks n seed

        Monthly _ ->
            Date.add Date.Months n seed

        Yearly _ ->
            Date.add Date.Years n seed


expandWithinPeriod : RecurrenceRule -> Date.Date -> Date.Date -> List Date.Date
expandWithinPeriod rule seed intervalDate =
    case rule.frequency of
        Secondly _ ->
            -- Sub-daily frequencies are handled by expandSubDaily, not the date-based pipeline
            []

        Minutely _ ->
            []

        Hourly _ ->
            []

        Daily _ ->
            [ intervalDate ]
                |> filterByMonth rule.byMonth
                |> filterByMonthDay rule.byMonthDay
                |> filterByDay rule.byDay
                |> filterByYearDay rule.byYearDay
                |> filterByWeekNo rule.byWeekNo

        Weekly { weekStart } ->
            (if List.isEmpty rule.byDay then
                [ intervalDate ]
                    |> filterByMonth rule.byMonth

             else
                expandWeekByDay weekStart rule.byDay intervalDate
                    |> filterByMonth rule.byMonth
            )
                |> filterByYearDay rule.byYearDay
                |> filterByWeekNo rule.byWeekNo

        Monthly _ ->
            expandMonthly rule intervalDate
                |> filterByYearDay rule.byYearDay
                |> filterByWeekNo rule.byWeekNo

        Yearly _ ->
            expandYearly rule seed intervalDate


daySpecWeekday : DaySpec -> Time.Weekday
daySpecWeekday spec =
    Recurrence.weekday spec


daySpecOrdinal : DaySpec -> Maybe Int
daySpecOrdinal spec =
    Recurrence.ordinal spec


expandWeekByDay : Time.Weekday -> List DaySpec -> Date.Date -> List Date.Date
expandWeekByDay weekStart byDay date =
    let
        weekStartDate : Date.Date
        weekStartDate =
            Date.floor (DateHelpers.weekdayToInterval weekStart) date
    in
    byDay
        |> List.filterMap
            (\spec ->
                let
                    target : Date.Date
                    target =
                        Date.ceiling (DateHelpers.weekdayToInterval (daySpecWeekday spec)) weekStartDate
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

        expandsAcrossAllMonths : Bool
        expandsAcrossAllMonths =
            not (List.isEmpty rule.byMonthDay) || not (List.isEmpty rule.byDay)

        months : List Time.Month
        months =
            if List.isEmpty rule.byMonth then
                if expandsAcrossAllMonths then
                    allMonths

                else
                    [ Date.month seed ]

            else
                rule.byMonth

        baseDates : List Date.Date
        baseDates =
            if not (List.isEmpty rule.byYearDay) then
                expandByYearDay year rule.byYearDay
                    |> filterByMonth rule.byMonth
                    |> filterByDay rule.byDay

            else if not (List.isEmpty rule.byWeekNo) then
                expandByWeekNo year rule.byWeekNo
                    |> filterByDay rule.byDay
                    |> filterByMonth rule.byMonth

            else if not (List.isEmpty rule.byMonthDay) then
                months
                    |> List.concatMap (\m -> expandByMonthDay year m rule.byMonthDay)
                    |> filterByDay rule.byDay

            else if not (List.isEmpty rule.byDay) then
                if List.isEmpty rule.byMonth then
                    expandByDayInYear year rule.byDay

                else
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
            DateHelpers.daysInMonth year month
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


expandByDayInYear : Int -> List DaySpec -> List Date.Date
expandByDayInYear year specs =
    specs
        |> List.concatMap (resolveDaySpecInYear year)
        |> List.sortBy Date.toRataDie


resolveDaySpecInMonth : Int -> Time.Month -> DaySpec -> List Date.Date
resolveDaySpecInMonth year month spec =
    let
        weekday : Time.Weekday
        weekday =
            daySpecWeekday spec

        interval : Date.Interval
        interval =
            DateHelpers.weekdayToInterval weekday
    in
    case daySpecOrdinal spec of
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
                        DateHelpers.daysInMonth year month

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


resolveDaySpecInYear : Int -> DaySpec -> List Date.Date
resolveDaySpecInYear year spec =
    let
        weekday : Time.Weekday
        weekday =
            daySpecWeekday spec

        matchingDates : List Date.Date
        matchingDates =
            allWeekdaysInYear year weekday
    in
    case daySpecOrdinal spec of
        Just n ->
            resolveNthDate n matchingDates
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        Nothing ->
            matchingDates


allWeekdaysInMonth : Time.Month -> Date.Date -> List Date.Date -> List Date.Date
allWeekdaysInMonth month current acc =
    if Date.month current /= month then
        List.reverse acc

    else
        allWeekdaysInMonth month (Date.add Date.Weeks 1 current) (current :: acc)


allWeekdaysInYear : Int -> Time.Weekday -> List Date.Date
allWeekdaysInYear year weekday =
    let
        jan1 : Date.Date
        jan1 =
            Date.fromCalendarDate year Time.Jan 1

        firstMatch : Date.Date
        firstMatch =
            Date.ceiling (DateHelpers.weekdayToInterval weekday) jan1
    in
    allWeekdaysInYearHelp year firstMatch []


allWeekdaysInYearHelp : Int -> Date.Date -> List Date.Date -> List Date.Date
allWeekdaysInYearHelp year current acc =
    if Date.year current /= year then
        List.reverse acc

    else
        allWeekdaysInYearHelp year (Date.add Date.Weeks 1 current) (current :: acc)


resolveNthDate : Int -> List Date.Date -> Maybe Date.Date
resolveNthDate n dates =
    if n > 0 then
        List.drop (n - 1) dates
            |> List.head

    else
        List.reverse dates
            |> List.drop (-n - 1)
            |> List.head


allMonths : List Time.Month
allMonths =
    [ Time.Jan
    , Time.Feb
    , Time.Mar
    , Time.Apr
    , Time.May
    , Time.Jun
    , Time.Jul
    , Time.Aug
    , Time.Sep
    , Time.Oct
    , Time.Nov
    , Time.Dec
    ]


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
                        DateHelpers.daysInMonth (Date.year d) (Date.month d)

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
                List.map daySpecWeekday byDay
        in
        List.filter (\d -> List.member (Date.weekday d) weekdays) dates


filterByYearDay : List Int -> List Date.Date -> List Date.Date
filterByYearDay byYearDay dates =
    if List.isEmpty byYearDay then
        dates

    else
        List.filter
            (\d ->
                let
                    totalDays : Int
                    totalDays =
                        if DateHelpers.isLeapYear (Date.year d) then
                            366

                        else
                            365

                    resolvedDays : List Int
                    resolvedDays =
                        List.filterMap
                            (\yd ->
                                let
                                    actual : Int
                                    actual =
                                        if yd < 0 then
                                            totalDays + yd + 1

                                        else
                                            yd
                                in
                                if actual >= 1 && actual <= totalDays then
                                    Just actual

                                else
                                    Nothing
                            )
                            byYearDay
                in
                List.member (Date.ordinalDay d) resolvedDays
            )
            dates


filterByWeekNo : List Int -> List Date.Date -> List Date.Date
filterByWeekNo byWeekNo dates =
    if List.isEmpty byWeekNo then
        dates

    else
        List.filter
            (\d ->
                let
                    totalWeeks : Int
                    totalWeeks =
                        isoWeeksInYear (Date.year d)

                    resolvedWeeks : List Int
                    resolvedWeeks =
                        List.filterMap
                            (\wn ->
                                let
                                    actual : Int
                                    actual =
                                        if wn < 0 then
                                            totalWeeks + wn + 1

                                        else
                                            wn
                                in
                                if actual >= 1 && actual <= totalWeeks then
                                    Just actual

                                else
                                    Nothing
                            )
                            byWeekNo
                in
                List.member (Date.weekNumber d) resolvedWeeks
            )
            dates


expandByYearDay : Int -> List Int -> List Date.Date
expandByYearDay year yearDays =
    let
        totalDays : Int
        totalDays =
            if DateHelpers.isLeapYear year then
                366

            else
                365

        jan1 : Date.Date
        jan1 =
            Date.fromCalendarDate year Time.Jan 1
    in
    yearDays
        |> List.filterMap
            (\yd ->
                let
                    actual : Int
                    actual =
                        if yd < 0 then
                            totalDays + yd + 1

                        else
                            yd
                in
                if actual >= 1 && actual <= totalDays then
                    Just (Date.add Date.Days (actual - 1) jan1)

                else
                    Nothing
            )
        |> List.sortBy Date.toRataDie


expandByWeekNo : Int -> List Int -> List Date.Date
expandByWeekNo year weekNos =
    let
        totalWeeks : Int
        totalWeeks =
            isoWeeksInYear year

        -- Find the Monday of ISO week 1
        jan4 : Date.Date
        jan4 =
            Date.fromCalendarDate year Time.Jan 4

        week1Monday : Date.Date
        week1Monday =
            Date.floor Date.Monday jan4
    in
    weekNos
        |> List.concatMap
            (\wn ->
                let
                    actual : Int
                    actual =
                        if wn < 0 then
                            totalWeeks + wn + 1

                        else
                            wn
                in
                if actual >= 1 && actual <= totalWeeks then
                    let
                        weekMonday : Date.Date
                        weekMonday =
                            Date.add Date.Weeks (actual - 1) week1Monday
                    in
                    -- Return all 7 days of that week
                    List.range 0 6
                        |> List.map (\i -> Date.add Date.Days i weekMonday)

                else
                    []
            )
        |> List.sortBy Date.toRataDie


dateToUnixMs : Date.Date -> Int
dateToUnixMs date =
    -- Rata Die epoch is Jan 1, 1 CE. Unix epoch (Jan 1, 1970) = Rata Die 719163
    (Date.toRataDie date - 719163) * 86400000


isoWeeksInYear : Int -> Int
isoWeeksInYear year =
    -- A year has 53 ISO weeks if Jan 1 is Thursday, or Dec 31 is Thursday
    let
        jan1Weekday : Time.Weekday
        jan1Weekday =
            Date.weekday (Date.fromCalendarDate year Time.Jan 1)

        dec31Weekday : Time.Weekday
        dec31Weekday =
            Date.weekday (Date.fromCalendarDate year Time.Dec 31)
    in
    if jan1Weekday == Time.Thu || dec31Weekday == Time.Thu then
        53

    else
        52


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


occurrenceFromRecurrenceDate : Event -> Time.Posix -> Occurrence
occurrenceFromRecurrenceDate event recurrenceDate =
    case event.time of
        WithTime { start, end } ->
            case ( start.timeZoneName, start.timeZoneContext ) of
                ( Just timeZoneName, Just timeZoneContext ) ->
                    let
                        startMs : Int
                        startMs =
                            Time.posixToMillis recurrenceDate

                        durationMs : Int
                        durationMs =
                            case end of
                                Just endTime ->
                                    Time.posixToMillis endTime.posix - Time.posixToMillis start.posix

                                Nothing ->
                                    0
                    in
                    { event = event
                    , time =
                        WithTime
                            { start = resolvedTimeFromPosixInTimeZone timeZoneName timeZoneContext recurrenceDate
                            , end =
                                Maybe.map
                                    (\_ ->
                                        resolvedTimeFromPosixInTimeZone
                                            timeZoneName
                                            timeZoneContext
                                            (Time.millisToPosix (startMs + durationMs))
                                    )
                                    end
                            }
                    }

                _ ->
                    let
                        startMs : Int
                        startMs =
                            Time.posixToMillis recurrenceDate

                        durationMs : Int
                        durationMs =
                            case end of
                                Just endTime ->
                                    Time.posixToMillis endTime.posix - Time.posixToMillis start.posix

                                Nothing ->
                                    0
                    in
                    { event = event
                    , time =
                        WithTime
                            { start =
                                { posix = recurrenceDate
                                , timeZoneName = start.timeZoneName
                                , localDateTime = Nothing
                                , timeZoneContext = start.timeZoneContext
                                }
                            , end =
                                Maybe.map
                                    (\_ ->
                                        { posix = Time.millisToPosix (startMs + durationMs)
                                        , timeZoneName = start.timeZoneName
                                        , localDateTime = Nothing
                                        , timeZoneContext = start.timeZoneContext
                                        }
                                    )
                                    end
                            }
                    }

        _ ->
            let
                seed : Date.Date
                seed =
                    occurrenceStartDate event.time
            in
            { event = event
            , time = shiftTime event.time seed (Date.fromPosix Time.utc recurrenceDate)
            }


occurrenceIsExcluded : Event -> Occurrence -> Bool
occurrenceIsExcluded event occurrence =
    if List.isEmpty event.exclusions then
        False

    else
        let
            occurrenceKey : Int
            occurrenceKey =
                occurrenceReferenceKeyFromOccurrence occurrence
        in
        event.exclusions
            |> List.any (\exclusion -> recurrenceReferenceKey occurrence.time exclusion == occurrenceKey)


occurrenceReferenceKeyFromOccurrence : Occurrence -> Int
occurrenceReferenceKeyFromOccurrence occurrence =
    case occurrence.time of
        WithTime { start } ->
            Time.posixToMillis start.posix

        _ ->
            Date.toRataDie (occurrenceStartDate occurrence.time)


recurrenceReferenceKey : EventTime -> Time.Posix -> Int
recurrenceReferenceKey eventTime recurrenceReference =
    case eventTime of
        WithTime _ ->
            Time.posixToMillis recurrenceReference

        _ ->
            Date.toRataDie (Date.fromPosix Time.utc recurrenceReference)


occurrenceStartDate : EventTime -> Date.Date
occurrenceStartDate eventTime =
    case eventTime of
        AllDay { start } ->
            start

        WithTime { start } ->
            case start.localDateTime of
                Just localDateTime ->
                    Date.fromCalendarDate localDateTime.year localDateTime.month localDateTime.day

                Nothing ->
                    Date.fromPosix Time.utc start.posix

        FloatingTime { start } ->
            Date.fromCalendarDate start.year start.month start.day


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
            case ( start.timeZoneName, start.localDateTime, start.timeZoneContext ) of
                ( Just timeZoneName, Just startLocalDateTime, Just timeZoneContext ) ->
                    let
                        durationMs : Int
                        durationMs =
                            case end of
                                Just endTime ->
                                    Time.posixToMillis endTime.posix - Time.posixToMillis start.posix

                                Nothing ->
                                    0

                        shiftedStart : Maybe ResolvedTime
                        shiftedStart =
                            resolveTimeZoneLocalDateTime timeZoneName timeZoneContext (shiftLocalDateTime dayDelta startLocalDateTime)

                        shiftedEnd : Maybe ResolvedTime
                        shiftedEnd =
                            case ( shiftedStart, end ) of
                                ( Just shiftedStartResolved, Just endResolved ) ->
                                    case endResolved.localDateTime of
                                        Just endLocalDateTime ->
                                            resolveTimeZoneLocalDateTime timeZoneName timeZoneContext (shiftLocalDateTime dayDelta endLocalDateTime)

                                        Nothing ->
                                            Just
                                                (resolvedTimeFromPosixInTimeZone
                                                    timeZoneName
                                                    timeZoneContext
                                                    (Time.millisToPosix (Time.posixToMillis shiftedStartResolved.posix + durationMs))
                                                )

                                _ ->
                                    Nothing
                    in
                    case shiftedStart of
                        Just shiftedStartResolved ->
                            WithTime
                                { start = shiftedStartResolved
                                , end = shiftedEnd
                                }

                        Nothing ->
                            let
                                deltaMs : Int
                                deltaMs =
                                    dayDelta * 86400000
                            in
                            WithTime
                                { start = { start | posix = Time.millisToPosix (Time.posixToMillis start.posix + deltaMs), localDateTime = Nothing }
                                , end =
                                    Maybe.map
                                        (\e -> { e | posix = Time.millisToPosix (Time.posixToMillis e.posix + deltaMs), localDateTime = Nothing })
                                        end
                                }

                _ ->
                    let
                        deltaMs : Int
                        deltaMs =
                            dayDelta * 86400000
                    in
                    WithTime
                        { start = { start | posix = Time.millisToPosix (Time.posixToMillis start.posix + deltaMs), localDateTime = Nothing }
                        , end =
                            Maybe.map
                                (\e -> { e | posix = Time.millisToPosix (Time.posixToMillis e.posix + deltaMs), localDateTime = Nothing })
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
            Date.fromCalendarDate ldt.year ldt.month ldt.day

        newDate : Date.Date
        newDate =
            Date.add Date.Days dayDelta originalDate
    in
    { year = Date.year newDate
    , month = Date.month newDate
    , day = Date.day newDate
    , hour = ldt.hour
    , minute = ldt.minute
    , second = ldt.second
    }


dedupOccurrences : List Occurrence -> List Occurrence
dedupOccurrences occurrences =
    dedupOccurrencesHelp occurrences -999999 []


dedupOccurrencesHelp : List Occurrence -> Int -> List Occurrence -> List Occurrence
dedupOccurrencesHelp remaining lastKey acc =
    case remaining of
        [] ->
            List.reverse acc

        occ :: rest ->
            let
                key : Int
                key =
                    occurrenceTimeKey occ.time
            in
            if key == lastKey then
                dedupOccurrencesHelp rest lastKey acc

            else
                dedupOccurrencesHelp rest key (occ :: acc)


occurrenceTimeKey : EventTime -> Int
occurrenceTimeKey eventTime =
    case eventTime of
        WithTime { start } ->
            Time.posixToMillis start.posix

        AllDay { start } ->
            Date.toRataDie start

        FloatingTime { start } ->
            Date.toRataDie (Date.fromCalendarDate start.year start.month start.day)
                * 86400
                + start.hour
                * 3600
                + start.minute
                * 60
                + start.second
