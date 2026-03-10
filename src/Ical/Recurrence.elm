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

    { frequency = Weekly
    , interval = 1
    , end = Forever
    , byDay = [ Every Time.Mon ]
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
    , byMonth : List Time.Month
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
