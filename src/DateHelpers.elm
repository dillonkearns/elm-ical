module DateHelpers exposing (daysInMonth, isLeapYear, weekdayToInterval)

{-| Internal helpers for date/time calculations shared across modules.
-}

import Date
import Time


{-| Convert a Time.Weekday to a Date.Interval for use with Date.ceiling/floor.
-}
weekdayToInterval : Time.Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday

        Time.Sun ->
            Date.Sunday


{-| Return the number of days in the given month for the given year.
-}
daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


{-| Check whether a year is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)
