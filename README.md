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
import Time


calendarFeed : String
calendarFeed =
    let
        calendarConfig =
            Ical.config
                { id = "//myapp//calendar//EN"
                , domain = "example.com"
                }
                |> Ical.withName "My Calendar"

        meeting =
            Ical.event
                { id = "meeting-123"
                , stamp = Time.millisToPosix 1633388093000
                , time =
                    Ical.WithTime
                        { start = Time.millisToPosix 1633384770000
                        , end = Time.millisToPosix 1633388370000
                        }
                , summary = "Team Meeting"
                }
                |> Ical.withDescription "Weekly sync"
                |> Ical.withLocation "Conference Room A"

        holiday =
            Ical.event
                { id = "holiday-456"
                , stamp = Time.millisToPosix 1633388093000
                , time =
                    Ical.AllDay
                        { start = Date.fromCalendarDate 2021 Time.Dec 25
                        , end = Date.fromCalendarDate 2021 Time.Dec 25
                        }
                , summary = "Christmas Day"
                }
                |> Ical.withStatus Ical.Confirmed
                |> Ical.withTransparency Ical.Transparent
    in
    Ical.generate calendarConfig [ meeting, holiday ]
```

## Parsing

```elm
import Ical.Parser as Parser


parseCalendar : String -> Result String (List String)
parseCalendar icsString =
    Parser.parse icsString
        |> Result.map
            (\cal ->
                List.filterMap .summary cal.events
            )
```

## What You Get

- Strict parsing. Malformed content lines, invalid dates/times/durations/RRULE values, and invalid `DTEND`/`DURATION` combinations return `Err`.
- Typed event times. Parsed events are `AllDay`, `WithTime`, or `FloatingTime`.
- TZID-aware parsing. `DATE-TIME` values can be resolved through matching `VTIMEZONE` data.
- Typed enums. `STATUS`, `TRANSP`, attendee role, and participation status are decoded into Elm union types.
- Unknown-property preservation. Unhandled and extension properties are kept in `extraProperties`.

## Important Semantics

- `Ical.EventTime.AllDay` uses an inclusive end date when generating.
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

### Parsing (`Ical.Parser`)

- Calendar properties: `PRODID`, `VERSION`, plus unknown calendar properties in `extraProperties`
- Event properties: `DTSTART`, `DTEND`, `DTSTAMP`, `DURATION`, `UID`, `SUMMARY`, `DESCRIPTION`, `LOCATION`, `ORGANIZER`, `STATUS`, `TRANSP`, `CREATED`, `LAST-MODIFIED`, `RRULE`, `EXDATE`, `ATTENDEE`
- Event times:
  - all-day dates
  - UTC date-times
  - floating local date-times
  - `TZID` date-times resolved through matching `VTIMEZONE`
- Attendees:
  - generation emits `CN` + `mailto:`
  - parsing reads `CN`, `ROLE`, `PARTSTAT`, and `RSVP`

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

- If a `TZID` value does not have a matching `VTIMEZONE`, it is currently kept as a `FloatingTime` value instead of being resolved through IANA data.
- `VTIMEZONE` support is intended for practical real-world feeds, but it is still not a complete implementation of every valid observance rule shape allowed by RFC 5545.
- The generation API still exposes some invalid states directly, notably `EventTime(..)` and the raw `RecurrenceRule` record in `Ical.Recurrence`.
