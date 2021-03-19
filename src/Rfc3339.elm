module Rfc3339 exposing (format, formatDateISO8601_2004)

import Date exposing (Date)
import DateFormat
import Time exposing (Posix)


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


{-| <https://tools.ietf.org/html/rfc5545#section-3.3.4>
-}
formatDateISO8601_2004 : Date -> String
formatDateISO8601_2004 date =
    date |> Date.format "yyyyMMdd"
