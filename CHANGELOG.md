# Changelog

## 1.0.0 - 2026-03-09

- Initial public release.
- Strict parsing for malformed content lines, invalid date/time values, invalid durations, and invalid `RRULE` parts.
- Correct `UNTIL` handling at date-time precision during recurrence expansion.
- Correct `EXDATE` handling for all-day, floating, and resolved date-time events.
- More complete `VTIMEZONE` support, including historical observances, `RDATE`, `UNTIL`, spring-forward gaps, and second-level UTC offsets.
- RFC default end semantics for `VEVENT`s that omit both `DTEND` and `DURATION`.
