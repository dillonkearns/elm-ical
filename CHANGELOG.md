# Changelog

## 1.0.0

Initial release.

### Generation (`Ical`)

- Generate `VCALENDAR`/`VEVENT` feeds from typed Elm values
- Calendar properties: `VERSION`, `PRODID`, `NAME`, `DESCRIPTION`, `URL`
- Event properties: `DTSTART`, `DTEND`, `DTSTAMP`, `UID`, `SUMMARY`, `DESCRIPTION`, `LOCATION`, `ORGANIZER`, `X-ALT-DESC`, `STATUS`, `TRANSP`, `CREATED`, `LAST-MODIFIED`, `RRULE`, `ATTENDEE`
- Opaque `EventTime` with `allDay` and `withTime` constructors (reversed dates/times normalized automatically)
- Opaque `Rule` builder for recurrence rules with `Time.Month` for type-safe `BYMONTH`
- RFC-compliant CRLF output, UTF-8-aware line folding, text escaping, and RFC 6868 parameter value escaping

### Parsing (`Ical.Parser`)

- `parse : String -> Result String Calendar`
- Strict parsing: malformed content lines, invalid values, invalid `EXDATE`, negative durations, and `TZID` without matching `VTIMEZONE` all return `Err`
- Typed event times: `AllDay`, `WithTime`, `FloatingTime`
- `TZID` date-times resolved through `VTIMEZONE` definitions (historical observances, `RDATE`, `UNTIL`, spring-forward gaps)
- Typed enums: `Status`, `Transparency`, `AttendeeRole`, `ParticipationStatus`
- Attendee parsing: `CN`, `ROLE`, `PARTSTAT`, `RSVP`
- `DURATION` resolved into `end` field
- RFC default end semantics when `DTEND` and `DURATION` are both omitted
- Unknown/extension properties preserved in `extraProperties`

### Recurrence (`Ical.Recurrence`)

- Shared `RecurrenceRule` type with `List Time.Month` for `byMonth`
- `Frequency`: `Daily`, `Weekly`, `Monthly`, `Yearly`
- Rule parts: `INTERVAL`, `COUNT`, `UNTIL`, `BYDAY` (with ordinals), `BYMONTHDAY`, `BYMONTH`, `BYSETPOS`, `WKST`
- `EXDATE` support for all event time types
- Expansion: `expand` (date-range-bounded) and `expandNext` (next N occurrences)
