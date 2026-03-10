module Ical.Recurrence exposing
    ( RecurrenceRule, RecurrenceEnd(..)
    , Frequency(..), DaySpec(..)
    )

{-| Types for iCal recurrence rules
([RFC 5545 Section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)).

[`RecurrenceRule`](#RecurrenceRule) is the transparent record returned by
[`Ical.Parser`](Ical-Parser) for reading parsed recurrence data.
[`Frequency`](#Frequency) and [`DaySpec`](#DaySpec) are also used when
building rules for generation via [`Ical.Rule`](Ical#Rule).


## Parsed rule data

@docs RecurrenceRule, RecurrenceEnd


## Shared types

@docs Frequency, DaySpec

-}

import Date
import Time


{-| A parsed recurrence rule (RRULE). This is the transparent record returned
by [`Ical.Parser`](Ical-Parser) for reading parsed rule data. For constructing
rules for generation, use [`Ical.Rule`](Ical#Rule) and its builder functions.

    { frequency = Weekly { every = 1, weekStart = Time.Mon }
    , end = Forever
    , byDay = [ Every Time.Mon ]
    , byMonthDay = []
    , byMonth = []
    , bySetPos = []
    }

  - `byDay` — which days of the week (with optional ordinals for monthly/yearly).
  - `byMonthDay` — which days of the month (1–31, or -31 to -1 from month end).
  - `byMonth` — which months.
  - `bySetPos` — pick the Nth result from the set produced by the other `by*`
    parts within each frequency period. Positive counts from the start, negative
    from the end. For example, `BYDAY=MO,TU,WE,TH,FR` with `bySetPos = [ -1 ]`
    means "the last weekday" of each period.

-}
type alias RecurrenceRule =
    { frequency : Frequency
    , end : RecurrenceEnd
    , byDay : List DaySpec
    , byMonthDay : List Int
    , byMonth : List Time.Month
    , bySetPos : List Int
    }


{-| How often the event recurs. Each variant carries its own configuration:

  - `every` — the interval between occurrences (e.g. `every = 2` with `Weekly`
    means every other week). Must be at least 1.
  - `weekStart` — which day begins the week (only meaningful for `Weekly`).

-}
type Frequency
    = Daily { every : Int }
    | Weekly { every : Int, weekStart : Time.Weekday }
    | Monthly { every : Int }
    | Yearly { every : Int }


{-| When the recurrence stops. `Count` and `Until*` are mutually exclusive per
[RFC 5545 Section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10).
-}
type RecurrenceEnd
    = Forever
    | Count Int
    | UntilDate Date.Date
    | UntilDateTime Time.Posix


{-| A day-of-week specifier for recurrence rules.

    -- every Monday
    Every Time.Mon

    -- the 2nd Sunday of the month
    Every2nd Time.Sun

    -- the last Friday of the month
    EveryLast Time.Fri

Positive ordinals (`Every1st` through `Every5th`) count from the start of the
month. Negative ordinals (`EveryLast` through `Every5thToLast`) count from the
end.

-}
type DaySpec
    = Every Time.Weekday
    | Every1st Time.Weekday
    | Every2nd Time.Weekday
    | Every3rd Time.Weekday
    | Every4th Time.Weekday
    | Every5th Time.Weekday
    | EveryLast Time.Weekday
    | Every2ndToLast Time.Weekday
    | Every3rdToLast Time.Weekday
    | Every4thToLast Time.Weekday
    | Every5thToLast Time.Weekday
