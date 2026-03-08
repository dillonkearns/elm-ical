# elm-ical

Generate and parse [iCal (RFC 5545)](https://datatracker.ietf.org/doc/html/rfc5545) calendar feeds in Elm.

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

The parser returns precise types:

- **`EventTime`** enforces that start and end share the same value type (`AllDay`, `WithTime`, or `FloatingTime`)
- **VTIMEZONE** datetimes are automatically resolved to `Time.Posix`
- **DURATION** is resolved into an end time (same semantics as DTEND)
- **`Status`** and **`Transparency`** are typed enums, not raw strings
- Unknown and extension properties (`X-*`) are preserved in `extraProperties`

## Supported Properties

### Calendar (VCALENDAR)

- `VERSION` (always 2.0)
- `PRODID`
- `NAME`
- `DESCRIPTION`
- `URL`

### Event (VEVENT)

- `DTSTART` / `DTEND` (DATE or DATE-TIME)
- `DTSTAMP`
- `DURATION` (parsing only — resolved into end time)
- `UID`
- `SUMMARY`
- `DESCRIPTION`
- `LOCATION`
- `ORGANIZER` (with CN parameter)
- `X-ALT-DESC` (HTML description with FMTTYPE parameter, generation only)
- `STATUS` (TENTATIVE, CONFIRMED, CANCELLED)
- `TRANSP` (OPAQUE, TRANSPARENT)
- `CREATED`
- `LAST-MODIFIED`
- `VTIMEZONE` (parsing only — used to resolve TZID datetimes)

## Not Yet Supported

- `RRULE` (recurrence rules — preserved in `extraProperties` when parsing)
- `VALARM` (alarms/reminders — skipped when parsing)
- `ATTENDEE`
- `VTODO` / `VJOURNAL`
