module Ical exposing
    ( generate, generateWithJournals, generateEvent
    , Config, config, withName, withCalendarDescription, withUrl
    , Event, event, EventTime, allDay, allDayRange, withTime, floatingTime, FloatingDateTime, Organizer, Attendee
    , withDescription, withLocation, withOrganizer, withHtmlDescription
    , withStatus, Status(..), withTransparency, Transparency(..)
    , withCreated, withLastModified
    , withRecurrenceRule, withAttendee
    , Journal, journal, JournalTime(..)
    , withJournalDescription, withJournalDate, withJournalTime, withJournalFloatingTime
    , withJournalStatus, JournalStatus(..)
    , withJournalCreated, withJournalLastModified, withJournalOrganizer
    , Rule, rule, withCount, withUntilDate, withUntilDateTime
    , withByDay, withByMonthDay, withByMonth, withBySetPos
    )

{-| Generate iCal ([RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545)) calendar feeds
from typed Elm values.

    import Date
    import Ical
    import Ical.Recurrence as Recurrence
    import Time

    teamCalendar : Time.Posix -> String
    teamCalendar now =
        let
            weeklySync : Ical.Event
            weeklySync =
                Ical.event
                    { id = "weekly-sync"
                    , stamp = now
                    , time =
                        Ical.withTime
                            { start = mar18at10am
                            , end = mar18at11am
                            }
                    , summary = "Weekly Team Sync"
                    }
                    |> Ical.withRecurrenceRule
                        (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon }))

            offsite : Ical.Event
            offsite =
                Ical.event
                    { id = "offsite-q2"
                    , stamp = now
                    , time =
                        Ical.allDayRange
                            { start = Date.fromCalendarDate 2021 Time.Jun 14
                            , end = Date.fromCalendarDate 2021 Time.Jun 16
                            }
                    , summary = "Q2 Team Offsite"
                    }
                    |> Ical.withLocation "Moscone Center, 747 Howard St, San Francisco, CA 94103"
        in
        Ical.generate
            (Ical.config
                { id = "//mycompany//team//EN"
                , domain = "mycompany.com"
                }
                |> Ical.withName "Engineering Team"
            )
            [ weeklySync, offsite ]

    mar18at10am : Time.Posix
    mar18at10am =
        Time.millisToPosix 1616065200000

    mar18at11am : Time.Posix
    mar18at11am =
        Time.millisToPosix 1616068800000

All generation types are opaque with builder functions. Invalid inputs like
reversed start/end times or negative intervals are silently normalized.


## Generating output

@docs generate, generateWithJournals, generateEvent


## Calendar configuration

@docs Config, config, withName, withCalendarDescription, withUrl


## Events

@docs Event, event, EventTime, allDay, allDayRange, withTime, floatingTime, FloatingDateTime, Organizer, Attendee
@docs withDescription, withLocation, withOrganizer, withHtmlDescription
@docs withStatus, Status, withTransparency, Transparency
@docs withCreated, withLastModified
@docs withRecurrenceRule, withAttendee


## Journals

@docs Journal, journal, JournalTime
@docs withJournalDescription, withJournalDate, withJournalTime, withJournalFloatingTime
@docs withJournalStatus, JournalStatus
@docs withJournalCreated, withJournalLastModified, withJournalOrganizer


## Recurrence rules

@docs Rule, rule, withCount, withUntilDate, withUntilDateTime
@docs withByDay, withByMonthDay, withByMonth, withBySetPos

-}

import Date exposing (Date)
import Ical.Recurrence as Recurrence exposing (DaySpec, Frequency(..))
import IcalDateTime
import Property exposing (Parameter(..), ValueData(..))
import Time


{-| Represents the time span of an event. Create values with [`allDay`](#allDay),
[`allDayRange`](#allDayRange), [`withTime`](#withTime), or
[`floatingTime`](#floatingTime).
-}
type EventTime
    = AllDay { start : Date, end : Date }
    | WithTime { start : Time.Posix, end : Time.Posix }
    | FloatingTime { start : FloatingDateTime, end : FloatingDateTime }


{-| A local date-time with no timezone. Used for floating-time events that
represent the same wall-clock time regardless of the viewer's timezone.

    { date = Date.fromCalendarDate 2021 Time.Mar 18
    , hour = 14
    , minute = 0
    , second = 0
    }

Out-of-range values are clamped: hour to 0–23, minute to 0–59, second to 0–59.

-}
type alias FloatingDateTime =
    { date : Date
    , hour : Int
    , minute : Int
    , second : Int
    }


{-| Create a single-day all-day event.

    Ical.allDay (Date.fromCalendarDate 2021 Time.Mar 18)

The library automatically adds one day to produce the exclusive DTEND
required by iCal.

-}
allDay : Date -> EventTime
allDay date =
    AllDay { start = date, end = date }


{-| Create a multi-day all-day event time span. The `end` date is **inclusive**.
The library automatically adds one day to produce the exclusive DTEND
required by iCal.

    Ical.allDayRange
        { start = Date.fromCalendarDate 2021 Time.Jun 14
        , end = Date.fromCalendarDate 2021 Time.Jun 16
        }

If `end` is before `start`, the dates are swapped automatically.

-}
allDayRange : { start : Date, end : Date } -> EventTime
allDayRange { start, end } =
    if Date.compare start end == GT then
        AllDay { start = end, end = start }

    else
        AllDay { start = start, end = end }


{-| Create a timed event time span with UTC start and end times.

If `end` is before `start`, the times are swapped automatically.

-}
withTime : { start : Time.Posix, end : Time.Posix } -> EventTime
withTime { start, end } =
    if Time.posixToMillis start > Time.posixToMillis end then
        WithTime { start = end, end = start }

    else
        WithTime { start = start, end = end }


{-| Create a floating-time event — a local date-time with no timezone.
The event represents the same wall-clock time regardless of the viewer's
timezone (e.g. "meeting at 2pm, wherever you are").

    Ical.floatingTime
        { start = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 14, minute = 0, second = 0 }
        , end = { date = Date.fromCalendarDate 2021 Time.Mar 18, hour = 15, minute = 30, second = 0 }
        }

If `end` is before `start`, the times are swapped automatically.
Out-of-range hour/minute/second values are clamped.

-}
floatingTime : { start : FloatingDateTime, end : FloatingDateTime } -> EventTime
floatingTime { start, end } =
    let
        clampedStart : FloatingDateTime
        clampedStart =
            clampFloatingDateTime start

        clampedEnd : FloatingDateTime
        clampedEnd =
            clampFloatingDateTime end
    in
    if compareFloatingDateTime clampedStart clampedEnd == GT then
        FloatingTime { start = clampedEnd, end = clampedStart }

    else
        FloatingTime { start = clampedStart, end = clampedEnd }


clampFloatingDateTime : FloatingDateTime -> FloatingDateTime
clampFloatingDateTime fdt =
    { date = fdt.date
    , hour = clamp 0 23 fdt.hour
    , minute = clamp 0 59 fdt.minute
    , second = clamp 0 59 fdt.second
    }


compareFloatingDateTime : FloatingDateTime -> FloatingDateTime -> Order
compareFloatingDateTime a b =
    let
        toComparable : FloatingDateTime -> ( Int, ( Int, Int, Int ) )
        toComparable fdt =
            ( Date.toRataDie fdt.date, ( fdt.hour, fdt.minute, fdt.second ) )
    in
    compare (toComparable a) (toComparable b)


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
    , recurrenceRule : Maybe Rule
    , attendees : List Attendee
    }


{-| Whether the event blocks time on a calendar for free/busy lookups.

  - `Opaque` — the event blocks time (default in iCal).
  - `Transparent` — the event does not block time (e.g. reminders, FYI events).

-}
type Transparency
    = Opaque
    | Transparent


{-| The scheduling status of an event.

  - `Tentative` — the event is not yet confirmed.
  - `Confirmed` — the event is definite.
  - `Cancelled` — the event has been cancelled.

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


{-| A person with a name and email address, used for the ATTENDEE property.
-}
type alias Attendee =
    { name : String
    , email : String
    }


{-| An opaque recurrence rule. Create one with [`rule`](#rule) and customize
with the `with*` builder functions.

    Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
        |> Ical.withByDay [ Recurrence.Every Time.Mon ]
        |> Ical.withCount 10

-}
type Rule
    = Rule RuleData


type alias RuleData =
    { frequency : Frequency
    , end : Recurrence.RecurrenceEnd
    , byDay : List DaySpec
    , byMonthDay : List Int
    , byMonth : List Time.Month
    , bySetPos : List Int
    }


{-| Create a recurrence rule with the given frequency. Negative `every` values
are clamped to 1.

    Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
        |> Ical.withByDay [ Recurrence.Every Time.Mon ]
        |> Ical.withCount 10

-}
rule : Recurrence.Frequency -> Rule
rule frequency =
    Rule
        { frequency = clampFrequencyEvery frequency
        , end = Recurrence.Forever
        , byDay = []
        , byMonthDay = []
        , byMonth = []
        , bySetPos = []
        }


frequencyEvery : Frequency -> Int
frequencyEvery freq =
    case freq of
        Daily { every } ->
            every

        Weekly { every } ->
            every

        Monthly { every } ->
            every

        Yearly { every } ->
            every


clampFrequencyEvery : Frequency -> Frequency
clampFrequencyEvery frequency =
    case frequency of
        Daily { every } ->
            Daily { every = max 1 every }

        Weekly { every, weekStart } ->
            Weekly { every = max 1 every, weekStart = weekStart }

        Monthly { every } ->
            Monthly { every = max 1 every }

        Yearly { every } ->
            Yearly { every = max 1 every }


{-| Set a count limit. Values less than 1 are clamped to 1.
-}
withCount : Int -> Rule -> Rule
withCount n (Rule r) =
    Rule { r | end = Recurrence.Count (max 1 n) }


{-| Set the recurrence end to a specific date.
-}
withUntilDate : Date -> Rule -> Rule
withUntilDate date (Rule r) =
    Rule { r | end = Recurrence.UntilDate date }


{-| Set the recurrence end to a specific date-time.
-}
withUntilDateTime : Time.Posix -> Rule -> Rule
withUntilDateTime posix (Rule r) =
    Rule { r | end = Recurrence.UntilDateTime posix }


{-| Set which days of the week the rule applies to.
-}
withByDay : List DaySpec -> Rule -> Rule
withByDay days (Rule r) =
    Rule { r | byDay = days }


{-| Set which days of the month the rule applies to. Valid values are
-31 to -1 and 1 to 31. Negative values count from the end of the month
(e.g. -1 is the last day).
-}
withByMonthDay : List Int -> Rule -> Rule
withByMonthDay days (Rule r) =
    Rule { r | byMonthDay = days }


{-| Set which months the rule applies to.

    Ical.rule (Recurrence.Yearly { every = 1 })
        |> Ical.withByMonth [ Time.Jan, Time.Apr, Time.Jul, Time.Oct ]

-}
withByMonth : List Time.Month -> Rule -> Rule
withByMonth months (Rule r) =
    Rule { r | byMonth = months }


{-| Filter to specific positions within each recurrence period. For example,
with `BYDAY=MO,TU,WE,TH,FR`, a `bySetPos` of `[ -1 ]` means "the last weekday."
-}
withBySetPos : List Int -> Rule -> Rule
withBySetPos positions (Rule r) =
    Rule { r | bySetPos = positions }


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

  - `id` — identifies your product in the PRODID property
    (e.g. `"//mycompany//myapp//EN"`).
  - `domain` — your domain, used to generate globally unique event UIDs
    (e.g. `"example.com"` produces UIDs like `"event-id@example.com"`).

```
Ical.config { id = "//mycompany//myapp//EN", domain = "example.com" }
```

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


{-| Set the calendar display name.
-}
withName : String -> Config -> Config
withName name (Config c) =
    Config { c | name = Just name }


{-| Set the calendar description.
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
        , time = Ical.withTime { start = startTime, end = endTime }
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


{-| Set a plain-text description for the event.
-}
withDescription : String -> Event -> Event
withDescription description (Event e) =
    Event { e | description = Just description }


{-| Set the event location (LOCATION property). Use a full, resolved address
so calendar apps can map it directly:

    Ical.event
        { id = "offsite-q2"
        , stamp = now
        , time = Ical.allDayRange { start = jun14, end = jun16 }
        , summary = "Q2 Team Offsite"
        }
        |> Ical.withLocation "Moscone Center, 747 Howard St, San Francisco, CA 94103"

-}
withLocation : String -> Event -> Event
withLocation location (Event e) =
    Event { e | location = Just location }


{-| Set the event organizer.
-}
withOrganizer : Organizer -> Event -> Event
withOrganizer organizer (Event e) =
    Event { e | organizer = Just organizer }


{-| Set an HTML-formatted description. Calendar apps that support rich text
will display this instead of the plain-text description.
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


{-| Set the creation timestamp.
-}
withCreated : Time.Posix -> Event -> Event
withCreated created (Event e) =
    Event { e | created = Just created }


{-| Set the last-modified timestamp.
-}
withLastModified : Time.Posix -> Event -> Event
withLastModified lastModified (Event e) =
    Event { e | lastModified = Just lastModified }


{-| Add a recurrence rule to the event.

    Ical.event { ... }
        |> Ical.withRecurrenceRule
            (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                |> Ical.withCount 10
            )

-}
withRecurrenceRule : Rule -> Event -> Event
withRecurrenceRule rrule (Event e) =
    Event { e | recurrenceRule = Just rrule }


{-| Add an attendee to the event.

    Ical.event { ... }
        |> Ical.withAttendee { name = "Jane Smith", email = "jane@example.com" }

-}
withAttendee : Attendee -> Event -> Event
withAttendee attendee (Event e) =
    Event { e | attendees = e.attendees ++ [ attendee ] }



-- Journal


{-| An opaque type representing a journal entry. Create one with
[`journal`](#journal) and customize with the `withJournal*` functions.
-}
type Journal
    = Journal JournalData


type alias JournalData =
    { id : String
    , stamp : Time.Posix
    , summary : String
    , description : Maybe String
    , time : Maybe JournalTime
    , status : Maybe JournalStatus
    , created : Maybe Time.Posix
    , lastModified : Maybe Time.Posix
    , organizer : Maybe Organizer
    }


{-| The time of a journal entry. Journals have no time span, just an optional
date or datetime stamp.
-}
type JournalTime
    = JournalAllDay Date
    | JournalWithTime Time.Posix
    | JournalFloatingTime FloatingDateTime


{-| Journal status per
[RFC 5545 Section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11).
-}
type JournalStatus
    = Draft
    | Final
    | CancelledJournal


{-| Create a journal entry with the required fields.

    Ical.journal
        { id = "journal-1"
        , stamp = timestamp
        , summary = "Daily standup notes"
        }

-}
journal : { id : String, stamp : Time.Posix, summary : String } -> Journal
journal { id, stamp, summary } =
    Journal
        { id = id
        , stamp = stamp
        , summary = summary
        , description = Nothing
        , time = Nothing
        , status = Nothing
        , created = Nothing
        , lastModified = Nothing
        , organizer = Nothing
        }


{-| Set a plain-text description for the journal entry.
-}
withJournalDescription : String -> Journal -> Journal
withJournalDescription description (Journal j) =
    Journal { j | description = Just description }


{-| Set the journal date (all-day).
-}
withJournalDate : Date -> Journal -> Journal
withJournalDate date (Journal j) =
    Journal { j | time = Just (JournalAllDay date) }


{-| Set the journal time (UTC).
-}
withJournalTime : Time.Posix -> Journal -> Journal
withJournalTime posix (Journal j) =
    Journal { j | time = Just (JournalWithTime posix) }


{-| Set the journal time as a floating local datetime.
-}
withJournalFloatingTime : FloatingDateTime -> Journal -> Journal
withJournalFloatingTime fdt (Journal j) =
    Journal { j | time = Just (JournalFloatingTime (clampFloatingDateTime fdt)) }


{-| Set the journal status.
-}
withJournalStatus : JournalStatus -> Journal -> Journal
withJournalStatus status (Journal j) =
    Journal { j | status = Just status }


{-| Set the journal creation timestamp.
-}
withJournalCreated : Time.Posix -> Journal -> Journal
withJournalCreated created (Journal j) =
    Journal { j | created = Just created }


{-| Set the journal last-modified timestamp.
-}
withJournalLastModified : Time.Posix -> Journal -> Journal
withJournalLastModified lastModified (Journal j) =
    Journal { j | lastModified = Just lastModified }


{-| Set the journal organizer.
-}
withJournalOrganizer : Organizer -> Journal -> Journal
withJournalOrganizer organizer (Journal j) =
    Journal { j | organizer = Just organizer }



-- Generating output


{-| Generate a complete iCal calendar string with the given config and events.
-}
generate : Config -> List Event -> String
generate cfg events =
    generateWithJournals cfg { events = events, journals = [] }


{-| Generate a complete iCal calendar string with events and journals.

    Ical.generateWithJournals
        (Ical.config { id = "//myapp//EN", domain = "example.com" })
        { events = [ weeklySync ]
        , journals = [ dailyNotes ]
        }

-}
generateWithJournals : Config -> { events : List Event, journals : List Journal } -> String
generateWithJournals ((Config c) as cfg) { events, journals } =
    let
        components : List String
        components =
            List.map (generateEvent cfg) events
                ++ List.map (generateJournal cfg) journals

        componentSection : String
        componentSection =
            case components of
                [] ->
                    ""

                _ ->
                    "\u{000D}\n" ++ String.join "\u{000D}\n" components
    in
    "BEGIN:VCALENDAR\u{000D}\n"
        ++ calendarProperties c
        ++ componentSection
        ++ "\u{000D}\nEND:VCALENDAR\u{000D}\n"


{-| Generate the iCal string for a single VEVENT component, without the
surrounding VCALENDAR wrapper. Useful for embedding in a larger calendar
you are building up. For a complete iCal document, use [`generate`](#generate).
-}
generateEvent : Config -> Event -> String
generateEvent (Config c) (Event details) =
    "BEGIN:VEVENT\u{000D}\n"
        ++ formatProperties (eventProperties c details)
        ++ "\u{000D}\nEND:VEVENT"


generateJournal : Config -> Journal -> String
generateJournal (Config c) (Journal details) =
    "BEGIN:VJOURNAL\u{000D}\n"
        ++ formatProperties (journalProperties c details)
        ++ "\u{000D}\nEND:VJOURNAL"


journalProperties : ConfigData -> JournalData -> List ( String, ValueData, List Parameter )
journalProperties c details =
    (case details.time of
        Just (JournalAllDay date) ->
            [ ( "DTSTART", Property.DateValue date, [ Parameter ( "VALUE", "DATE" ) ] ) ]

        Just (JournalWithTime posix) ->
            [ ( "DTSTART", Property.DateTime posix, [] ) ]

        Just (JournalFloatingTime fdt) ->
            [ ( "DTSTART", Property.FloatingDateTime fdt, [] ) ]

        Nothing ->
            []
    )
        ++ [ ( "DTSTAMP", details.stamp |> Property.DateTime, [] )
           , ( "UID", details.id ++ "@" ++ c.domain |> Text, [] )
           , ( "SUMMARY", details.summary |> Text, [] )
           ]
        ++ ([ details.created |> Maybe.map (\created -> ( "CREATED", created |> Property.DateTime, [] ))
            , details.lastModified |> Maybe.map (\lastModified -> ( "LAST-MODIFIED", lastModified |> Property.DateTime, [] ))
            , details.description |> Maybe.andThen nonEmpty |> Maybe.map (\description -> ( "DESCRIPTION", Text description, [] ))
            , details.status |> Maybe.map (\status -> ( "STATUS", status |> journalStatusToString |> Text, [] ))
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


journalStatusToString : JournalStatus -> String
journalStatusToString status =
    case status of
        Draft ->
            "DRAFT"

        Final ->
            "FINAL"

        CancelledJournal ->
            "CANCELLED"


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
                    (\(Rule r) ->
                        ( "RRULE"
                        , Uri (formatRule r)
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

        FloatingTime { start, end } ->
            [ ( "DTSTART", Property.FloatingDateTime start, [] )
            , ( "DTEND", Property.FloatingDateTime end, [] )
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


formatRule : RuleData -> String
formatRule r =
    let
        freqStr : String
        freqStr =
            case r.frequency of
                Daily _ ->
                    "DAILY"

                Weekly _ ->
                    "WEEKLY"

                Monthly _ ->
                    "MONTHLY"

                Yearly _ ->
                    "YEARLY"

        interval : Int
        interval =
            frequencyEvery r.frequency

        maybeWeekStart : Maybe Time.Weekday
        maybeWeekStart =
            case r.frequency of
                Weekly { weekStart } ->
                    if weekStart /= Time.Mon then
                        Just weekStart

                    else
                        Nothing

                _ ->
                    Nothing

        parts : List (Maybe String)
        parts =
            [ Just ("FREQ=" ++ freqStr)
            , if interval > 1 then
                Just ("INTERVAL=" ++ String.fromInt interval)

              else
                Nothing
            , case r.end of
                Recurrence.Forever ->
                    Nothing

                Recurrence.Count n ->
                    Just ("COUNT=" ++ String.fromInt n)

                Recurrence.UntilDate date ->
                    Just ("UNTIL=" ++ IcalDateTime.formatDate date)

                Recurrence.UntilDateTime posix ->
                    Just ("UNTIL=" ++ IcalDateTime.format posix)
            , if List.isEmpty r.byDay then
                Nothing

              else
                Just ("BYDAY=" ++ String.join "," (List.map formatDaySpec r.byDay))
            , if List.isEmpty r.byMonthDay then
                Nothing

              else
                Just ("BYMONTHDAY=" ++ String.join "," (List.map String.fromInt r.byMonthDay))
            , if List.isEmpty r.byMonth then
                Nothing

              else
                Just ("BYMONTH=" ++ String.join "," (List.map (Date.monthToNumber >> String.fromInt) r.byMonth))
            , if List.isEmpty r.bySetPos then
                Nothing

              else
                Just ("BYSETPOS=" ++ String.join "," (List.map String.fromInt r.bySetPos))
            , maybeWeekStart
                |> Maybe.map (\ws -> "WKST=" ++ weekdayToString ws)
            ]
    in
    parts
        |> List.filterMap identity
        |> String.join ";"


formatDaySpec : Recurrence.DaySpec -> String
formatDaySpec spec =
    let
        ( prefix, weekday ) =
            daySpecToOrdinalAndWeekday spec
    in
    prefix ++ weekdayToString weekday


daySpecToOrdinalAndWeekday : Recurrence.DaySpec -> ( String, Time.Weekday )
daySpecToOrdinalAndWeekday spec =
    case spec of
        Recurrence.Every wd ->
            ( "", wd )

        Recurrence.Every1st wd ->
            ( "1", wd )

        Recurrence.Every2nd wd ->
            ( "2", wd )

        Recurrence.Every3rd wd ->
            ( "3", wd )

        Recurrence.Every4th wd ->
            ( "4", wd )

        Recurrence.Every5th wd ->
            ( "5", wd )

        Recurrence.EveryLast wd ->
            ( "-1", wd )

        Recurrence.Every2ndToLast wd ->
            ( "-2", wd )

        Recurrence.Every3rdToLast wd ->
            ( "-3", wd )

        Recurrence.Every4thToLast wd ->
            ( "-4", wd )

        Recurrence.Every5thToLast wd ->
            ( "-5", wd )


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
