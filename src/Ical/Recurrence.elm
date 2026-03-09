module Ical.Recurrence exposing (RecurrenceRule, Frequency(..), RecurrenceEnd(..), DaySpec)

{-| Types for iCal recurrence rules
([RFC 5545 Section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)).

[`RecurrenceRule`](#RecurrenceRule) is the transparent record returned by
[`Ical.Parser`](Ical-Parser). [`Frequency`](#Frequency) and
[`DaySpec`](#DaySpec) are also used when building rules for generation
via [`Ical.Rule`](Ical#Rule).

@docs RecurrenceRule, Frequency, RecurrenceEnd, DaySpec

-}

import Date
import Time


{-| A parsed recurrence rule (RRULE). This is the transparent record returned
by [`Ical.Parser`](Ical-Parser) for reading parsed rule data. For constructing
rules for generation, use [`Ical.Rule`](Ical#Rule) and its builder functions.

    { frequency = Weekly
    , interval = 1
    , end = Forever
    , byDay = [ { ordinal = Nothing, weekday = Time.Mon } ]
    , byMonthDay = []
    , byMonth = []
    , bySetPos = []
    , weekStart = Time.Mon
    }

-}
type alias RecurrenceRule =
    { frequency : Frequency
    , interval : Int
    , end : RecurrenceEnd
    , byDay : List DaySpec
    , byMonthDay : List Int
    , byMonth : List Int
    , bySetPos : List Int
    , weekStart : Time.Weekday
    }


{-| How often the event recurs.
-}
type Frequency
    = Daily
    | Weekly
    | Monthly
    | Yearly


{-| When the recurrence stops. `Count` and `Until*` are mutually exclusive per
[RFC 5545 Section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10).
-}
type RecurrenceEnd
    = Forever
    | Count Int
    | UntilDate Date.Date
    | UntilDateTime Time.Posix


{-| A day-of-week specifier, optionally with an ordinal.

  - `{ ordinal = Nothing, weekday = Time.Mon }` — every Monday
  - `{ ordinal = Just 2, weekday = Time.Sun }` — the 2nd Sunday
  - `{ ordinal = Just -1, weekday = Time.Fri }` — the last Friday

-}
type alias DaySpec =
    { ordinal : Maybe Int
    , weekday : Time.Weekday
    }
