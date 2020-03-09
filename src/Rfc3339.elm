module Rfc3339 exposing (..)

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
