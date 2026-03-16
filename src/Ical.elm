module Ical exposing
    ( Status(..)
    , Transparency(..)
    , JournalStatus(..)
    )

{-| Shared types for the elm-ical package.

To generate iCal feeds, see [`Ical.Generator`](Ical-Generator).
To parse iCal feeds, see [`Ical.Parser`](Ical-Parser).
For recurrence rule types, see [`Ical.Recurrence`](Ical-Recurrence).


## Event status

@docs Status


## Transparency

@docs Transparency


## Journal status

@docs JournalStatus

-}


{-| The scheduling status of an event.

  - `Tentative` — the event is not yet confirmed.
  - `Confirmed` — the event is definite.
  - `Cancelled` — the event has been cancelled.

Per [RFC 5545 Section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11).

-}
type Status
    = Tentative
    | Confirmed
    | Cancelled


{-| Whether the event blocks time on a calendar for free/busy lookups.

  - `Opaque` — the event blocks time (default in iCal).
  - `Transparent` — the event does not block time (e.g. reminders, FYI events).

Per [RFC 5545 Section 3.8.2.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.7).

-}
type Transparency
    = Opaque
    | Transparent


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
