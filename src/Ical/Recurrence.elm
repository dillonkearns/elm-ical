module Ical.Recurrence exposing
    ( RecurrenceRule, RecurrenceEnd(..)
    , Frequency(..), DaySpec, every, nth, weekday, ordinal
    , first, second, third, fourth, fifth
    , last, secondToLast, thirdToLast, fourthToLast, fifthToLast
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

@docs Frequency, DaySpec, every, nth, weekday, ordinal
@docs first, second, third, fourth, fifth
@docs last, secondToLast, thirdToLast, fourthToLast, fifthToLast

-}

import Date
import Time


{-| A parsed recurrence rule (RRULE). This is the transparent record returned
by [`Ical.Parser`](Ical-Parser) for reading parsed rule data. For constructing
rules for generation, use [`Ical.Rule`](Ical#Rule) and its builder functions.

    { frequency = Weekly { every = 1, weekStart = Time.Mon }
    , end = Forever
    , byDay = [ every Time.Mon ]
    , byMonthDay = []
    , byMonth = []
    , bySetPos = []
    , byHour = []
    , byMinute = []
    , bySecond = []
    , byYearDay = []
    , byWeekNo = []
    }

  - `byDay` — which days of the week (with optional ordinals for monthly/yearly).
  - `byMonthDay` — which days of the month (1–31, or -31 to -1 from month end).
  - `byMonth` — which months.
  - `bySetPos` — pick the Nth result from the set produced by the other `by*`
    parts within each frequency period. Positive counts from the start, negative
    from the end. For example, `BYDAY=MO,TU,WE,TH,FR` with `bySetPos = [ -1 ]`
    means "the last weekday" of each period.
  - `byHour` — which hours (0–23).
  - `byMinute` — which minutes (0–59).
  - `bySecond` — which seconds (0–60, 60 for leap second).
  - `byYearDay` — which days of the year (1–366, or -366 to -1 from year end).
  - `byWeekNo` — which ISO week numbers (1–53, or -53 to -1 from year end).

-}
type alias RecurrenceRule =
    { frequency : Frequency
    , end : RecurrenceEnd
    , byDay : List DaySpec
    , byMonthDay : List Int
    , byMonth : List Time.Month
    , bySetPos : List Int
    , byHour : List Int
    , byMinute : List Int
    , bySecond : List Int
    , byYearDay : List Int
    , byWeekNo : List Int
    }


{-| How often the event recurs. Each variant carries its own configuration:

  - `every` — the interval between occurrences (e.g. `every = 2` with `Weekly`
    means every other week). Must be at least 1.
  - `weekStart` — which day begins the week (only meaningful for `Weekly`).

-}
type Frequency
    = Secondly { every : Int }
    | Minutely { every : Int }
    | Hourly { every : Int }
    | Daily { every : Int }
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

Create one with [`every`](#every), [`nth`](#nth), or the convenience helpers
like [`second`](#second) and [`last`](#last).

    -- every Monday
    every Time.Mon

    -- the 2nd Sunday of the month
    second Time.Sun

    -- the last Friday
    last Time.Fri

Positive ordinals count from the start of the period. Negative ordinals count
from the end. `nth` accepts `-53..-1` and `1..53`, matching the RFC grammar
for numeric `BYDAY` values.

-}
type DaySpec
    = Every Time.Weekday
    | Nth Int Time.Weekday


{-| Match every instance of the given weekday.
-}
every : Time.Weekday -> DaySpec
every day =
    Every day


{-| Match the `n`th instance of a weekday.

Positive values count from the start, negative values count from the end, and
`0` is invalid.

    nth 20 Time.Mon

    nth -1 Time.Fri

-}
nth : Int -> Time.Weekday -> Maybe DaySpec
nth n day =
    if isValidOrdinal n then
        Just (Nth n day)

    else
        Nothing


{-| Extract the weekday from a `DaySpec`.
-}
weekday : DaySpec -> Time.Weekday
weekday spec =
    case spec of
        Every day ->
            day

        Nth _ day ->
            day


{-| Extract the ordinal from a `DaySpec`, if present.
-}
ordinal : DaySpec -> Maybe Int
ordinal spec =
    case spec of
        Every _ ->
            Nothing

        Nth n _ ->
            Just n


{-| The 1st instance of a weekday.
-}
first : Time.Weekday -> DaySpec
first day =
    Nth 1 day


{-| The 2nd instance of a weekday.
-}
second : Time.Weekday -> DaySpec
second day =
    Nth 2 day


{-| The 3rd instance of a weekday.
-}
third : Time.Weekday -> DaySpec
third day =
    Nth 3 day


{-| The 4th instance of a weekday.
-}
fourth : Time.Weekday -> DaySpec
fourth day =
    Nth 4 day


{-| The 5th instance of a weekday.
-}
fifth : Time.Weekday -> DaySpec
fifth day =
    Nth 5 day


{-| The last instance of a weekday.
-}
last : Time.Weekday -> DaySpec
last day =
    Nth -1 day


{-| The 2nd-to-last instance of a weekday.
-}
secondToLast : Time.Weekday -> DaySpec
secondToLast day =
    Nth -2 day


{-| The 3rd-to-last instance of a weekday.
-}
thirdToLast : Time.Weekday -> DaySpec
thirdToLast day =
    Nth -3 day


{-| The 4th-to-last instance of a weekday.
-}
fourthToLast : Time.Weekday -> DaySpec
fourthToLast day =
    Nth -4 day


{-| The 5th-to-last instance of a weekday.
-}
fifthToLast : Time.Weekday -> DaySpec
fifthToLast day =
    Nth -5 day


isValidOrdinal : Int -> Bool
isValidOrdinal n =
    n /= 0 && abs n <= 53
