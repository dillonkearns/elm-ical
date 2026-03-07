module IcalDateTime exposing (format, formatDate)

import Date exposing (Date)
import DateFormat
import Time exposing (Posix)


{-| Format a Posix time as an iCal DATE-TIME value (e.g. "20210318T162044Z").
-}
format : Posix -> String
format posix =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.monthFixed
        , DateFormat.dayOfMonthFixed
        , DateFormat.text "T"
        , DateFormat.hourMilitaryFixed
        , DateFormat.minuteFixed
        , DateFormat.secondFixed
        , DateFormat.text "Z"
        ]
        Time.utc
        posix


{-| Format a Date as an iCal DATE value (e.g. "20210318").

<https://tools.ietf.org/html/rfc5545#section-3.3.4>

-}
formatDate : Date -> String
formatDate date =
    date |> Date.format "yyyyMMdd"
