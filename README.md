# elm-ical

Generate and parse a practical subset of [iCalendar (RFC 5545)](https://datatracker.ietf.org/doc/html/rfc5545) in Elm.

`elm-ical` is currently optimized for:

- generating `VCALENDAR` / `VEVENT` feeds from typed Elm values
- parsing `VEVENT` feeds strictly into typed Elm data
- expanding the common `RRULE` subset directly in Elm

It is not a complete RFC 5545 implementation yet. The current scope is calendar/event feeds plus the recurrence and timezone pieces needed to work with them reliably.

## Generating

```elm
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
                    (Ical.rule Recurrence.Weekly)

        offsite : Ical.Event
        offsite =
            Ical.event
                { id = "offsite-q2"
                , stamp = now
                , time =
                    Ical.allDay
                        { start = Date.fromCalendarDate 2021 Time.Jun 14
                        , end = Date.fromCalendarDate 2021 Time.Jun 16
                        }
                , summary = "Q2 Team Offsite"
                }
                |> Ical.withLocation "Moscone Center, 747 Howard St, San Francisco, CA 94103"
    in
    Ical.generate
        (Ical.config { id = "//mycompany//team//EN", domain = "mycompany.com" }
            |> Ical.withName "Engineering Team"
        )
        [ weeklySync, offsite ]

mar18at10am : Time.Posix
mar18at10am =
    Time.millisToPosix 1616065200000

mar18at11am : Time.Posix
mar18at11am =
    Time.millisToPosix 1616068800000
```

## Parsing

```elm
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
```

See [`examples/`](examples/) for a full runnable script that fetches and parses a live Google Calendar feed.

## What You Get

- Strict parsing. Malformed content lines, invalid dates/times/durations/RRULE values, and invalid `DTEND`/`DURATION` combinations return `Err`. A `TZID` parameter without a matching `VTIMEZONE` definition is a hard parse error.
- Typed event times. Parsed events are `AllDay`, `WithTime`, or `FloatingTime`.
- TZID-aware parsing. `DATE-TIME` values with a `TZID` parameter are resolved through matching `VTIMEZONE` data.
- Typed enums. `STATUS`, `TRANSP`, attendee role, and participation status are decoded into Elm union types.
- Unknown-property preservation. Unhandled and extension properties are kept in `extraProperties`.
- Invalid states prevented. Generation-side types like `EventTime` and `Rule` are opaque with builder functions that normalize inputs (e.g. reversed start/end dates are swapped, negative intervals are clamped to 1).

## Important Semantics

- `Ical.allDay` uses an inclusive end date when generating.
- `Ical.Parser.EventTime.AllDay` uses RFC-style exclusive end semantics when parsing.
- If a parsed `VEVENT` omits both `DTEND` and `DURATION`, the parser applies the RFC default:
  - all-day events end on the following date
  - timed and floating events end at the same instant they start

## Supported Today

### Generation (`Ical`)

- Calendar properties: `VERSION`, `PRODID`, `NAME` (non-standard but widely used), `DESCRIPTION`, `URL`
- Event properties: `DTSTART`, `DTEND`, `DTSTAMP`, `UID`, `SUMMARY`, `DESCRIPTION`, `LOCATION`, `ORGANIZER`, `X-ALT-DESC`, `STATUS`, `TRANSP`, `CREATED`, `LAST-MODIFIED`, `RRULE`, `ATTENDEE`
- Event times:
  - all-day events
  - UTC date-time events
- Recurrence rules via opaque `Rule` builder:
  - `FREQ`, `INTERVAL`, `COUNT`, `UNTIL`, `BYDAY`, `BYMONTHDAY`, `BYMONTH`, `BYSETPOS`, `WKST`
  - `BYMONTH` uses `Time.Month` values, so invalid months are impossible at the type level

### Parsing (`Ical.Parser`)

- Calendar properties: `PRODID`, `VERSION`, plus unknown calendar properties in `extraProperties`
- Event properties: `DTSTART`, `DTEND`, `DTSTAMP`, `DURATION`, `UID`, `SUMMARY`, `DESCRIPTION`, `LOCATION`, `ORGANIZER`, `STATUS`, `TRANSP`, `CREATED`, `LAST-MODIFIED`, `RRULE`, `EXDATE`, `ATTENDEE`
- Event times:
  - all-day dates
  - UTC date-times
  - floating local date-times
  - `TZID` date-times resolved through matching `VTIMEZONE`
- Attendees: `CN`, `ROLE`, `PARTSTAT`, `RSVP`

### Recurrence Subset

- Frequencies: `DAILY`, `WEEKLY`, `MONTHLY`, `YEARLY`
- Rule parts: `INTERVAL`, `COUNT`, `UNTIL`, `BYDAY`, `BYMONTHDAY`, `BYMONTH`, `BYSETPOS`, `WKST`
- Exception dates: `EXDATE` for all-day, floating, and resolved date-time events
- Expansion helpers: `Ical.Parser.expand` and `Ical.Parser.expandNext`

## Not Supported Yet

- Generating floating local times
- Generating `TZID` date-times or `VTIMEZONE` components
- `SECONDLY`, `MINUTELY`, `HOURLY`
- `BYSECOND`, `BYMINUTE`, `BYHOUR`, `BYYEARDAY`, `BYWEEKNO`
- Event-level `RDATE`
- `RECURRENCE-ID` and overridden instances
- Typed `VALARM` extraction
- `VTODO`, `VJOURNAL`, and `VFREEBUSY`

## Known Limitations

- `VTIMEZONE` support is intended for practical real-world feeds, but it is still not a complete implementation of every valid observance rule shape allowed by RFC 5545. The common yearly pattern (`FREQ=YEARLY;BYMONTH=...;BYDAY=...`) is supported, along with `RDATE`, `UNTIL`, and historical observance chains.
