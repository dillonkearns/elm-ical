# elm-ical

Generate [iCal (RFC 5545)](https://tools.ietf.org/html/rfc5545) calendar feeds in Elm.

## Usage

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
- `UID`
- `SUMMARY`
- `DESCRIPTION`
- `LOCATION`
- `ORGANIZER` (with CN parameter)
- `X-ALT-DESC` (HTML description with FMTTYPE parameter)
- `STATUS` (TENTATIVE, CONFIRMED, CANCELLED)
- `TRANSP` (OPAQUE, TRANSPARENT)
- `CREATED`
- `LAST-MODIFIED`

## Not Yet Supported

- `RRULE` (recurrence rules)
- `VALARM` (alarms/reminders)
- `ATTENDEE`
- `VTIMEZONE`
- `VTODO` / `VJOURNAL`
