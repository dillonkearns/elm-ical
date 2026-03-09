module Ical.Expansion exposing (Occurrence, expand, expandNext)

{-| Expand recurring events into individual occurrences.

@docs Occurrence, expand, expandNext

-}

import Date exposing (Date)
import Ical.Parser as Parser
import Ical.Recurrence exposing (DaySpec, Frequency(..), RecurrenceEnd(..), RecurrenceRule)
import Time


{-| A single occurrence of an event. For non-recurring events, there is one
occurrence whose `time` matches `event.time`. For recurring events, each
occurrence has its own `time` shifted to the recurrence date while preserving
the original event's duration.
-}
type alias Occurrence =
    { event : Parser.Event
    , time : Parser.EventTime
    }


{-| Expand an event's recurrence rules into a list of concrete occurrences
within a date range.

    expand { start = jan1, end = dec31 } event

For events without recurrence rules, returns a single occurrence if the event
falls within the range. For events with recurrence rules, generates all
occurrences within the range, respecting COUNT, UNTIL, EXDATE, and all BY\*
filters.

-}
expand : { start : Date, end : Date } -> Parser.Event -> List Occurrence
expand range event =
    case event.recurrenceRules of
        [] ->
            let
                eventDate : Date
                eventDate =
                    startDate event.time
            in
            if Date.compare eventDate range.start /= LT && Date.compare eventDate range.end /= GT then
                [ { event = event, time = event.time } ]

            else
                []

        rules ->
            rules
                |> List.concatMap (\rule -> expandRule range event rule)
                |> List.sortBy (\occ -> Date.toRataDie (startDate occ.time))
                |> dedup


{-| Get the next N occurrences of an event starting from a given date.

    expandNext 5 today event

For non-recurring events, returns a single occurrence if the event falls on or
after the start date, or an empty list if it is before.

-}
expandNext : Int -> Date -> Parser.Event -> List Occurrence
expandNext n fromDate event =
    case event.recurrenceRules of
        [] ->
            let
                eventDate : Date
                eventDate =
                    startDate event.time
            in
            if Date.compare eventDate fromDate /= LT then
                [ { event = event, time = event.time } ]

            else
                []

        rules ->
            rules
                |> List.concatMap (\rule -> expandNextRule n fromDate event rule)
                |> List.sortBy (\occ -> Date.toRataDie (startDate occ.time))
                |> dedup
                |> List.take n


expandNextRule : Int -> Date -> Parser.Event -> RecurrenceRule -> List Occurrence
expandNextRule n fromDate event rule =
    let
        seed : Date
        seed =
            startDate event.time

        fromRD : Int
        fromRD =
            Date.toRataDie fromDate
    in
    expandNextChunked rule seed fromRD n event 1 []


expandNextChunked : RecurrenceRule -> Date -> Int -> Int -> Parser.Event -> Int -> List Occurrence -> List Occurrence
expandNextChunked rule seed fromRD needed event chunkYears acc =
    let
        rangeEndRD : Int
        rangeEndRD =
            fromRD + (chunkYears * 366)

        candidates : List Date
        candidates =
            generateCandidates rule seed rangeEndRD

        filtered : List Date
        filtered =
            candidates
                |> filterExclusions event

        newOccurrences : List Occurrence
        newOccurrences =
            filtered
                |> List.filter (\d -> Date.toRataDie d >= fromRD)
                |> List.map (\d -> { event = event, time = shiftTime event.time seed d })

        combined : List Occurrence
        combined =
            acc ++ newOccurrences
    in
    if List.length combined >= needed then
        List.take needed combined

    else
        -- Check if the rule has a natural end (COUNT/UNTIL) and we've exhausted it
        case rule.end of
            Forever ->
                -- Try a larger chunk
                if chunkYears >= 100 then
                    combined

                else
                    expandNextChunked rule seed fromRD needed event (chunkYears * 2) []

            _ ->
                -- Rule has a natural end, we've generated all candidates
                combined


expandRule : { start : Date, end : Date } -> Parser.Event -> RecurrenceRule -> List Occurrence
expandRule range event rule =
    let
        seed : Date
        seed =
            startDate event.time

        rangeStartRD : Int
        rangeStartRD =
            Date.toRataDie range.start

        rangeEndRD : Int
        rangeEndRD =
            Date.toRataDie range.end

        candidates : List Date
        candidates =
            generateCandidates rule seed rangeEndRD

        filtered : List Date
        filtered =
            candidates
                |> filterExclusions event
    in
    filtered
        |> List.filter (\d -> Date.toRataDie d >= rangeStartRD && Date.toRataDie d <= rangeEndRD)
        |> List.map (\d -> { event = event, time = shiftTime event.time seed d })


generateCandidates : RecurrenceRule -> Date -> Int -> List Date
generateCandidates rule seed rangeEndRD =
    generateLoop rule seed rangeEndRD 0 0 []


generateLoop : RecurrenceRule -> Date -> Int -> Int -> Int -> List Date -> List Date
generateLoop rule seed rangeEndRD stepIndex emittedCount acc =
    let
        intervalDate : Date
        intervalDate =
            advanceByInterval rule.frequency (rule.interval * stepIndex) seed

        candidates : List Date
        candidates =
            expandWithinPeriod rule seed intervalDate
                |> applyBySetPos rule.bySetPos
    in
    if List.isEmpty candidates then
        -- If we've gone far past the range with no candidates, stop
        if Date.toRataDie intervalDate > rangeEndRD + 366 then
            List.reverse acc

        else
            generateLoop rule seed rangeEndRD (stepIndex + 1) emittedCount acc

    else
        let
            lowestCandidateRD : Int
            lowestCandidateRD =
                candidates
                    |> List.map Date.toRataDie
                    |> List.minimum
                    |> Maybe.withDefault (rangeEndRD + 1)
        in
        if lowestCandidateRD > rangeEndRD && not (isCountLimited rule.end) then
            List.reverse acc

        else
            collectCandidates rule seed rangeEndRD stepIndex emittedCount acc candidates


collectCandidates : RecurrenceRule -> Date -> Int -> Int -> Int -> List Date -> List Date -> List Date
collectCandidates rule seed rangeEndRD stepIndex emittedCount acc candidates =
    case candidates of
        [] ->
            generateLoop rule seed rangeEndRD (stepIndex + 1) emittedCount acc

        c :: rest ->
            -- Don't emit dates before the seed (DTSTART is always the first occurrence)
            if Date.toRataDie c < Date.toRataDie seed then
                collectCandidates rule seed rangeEndRD stepIndex emittedCount acc rest

            else
                case rule.end of
                    Count n ->
                        if emittedCount >= n then
                            List.reverse acc

                        else
                            collectCandidates rule seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    UntilDate untilDate ->
                        if Date.compare c untilDate == GT then
                            List.reverse acc

                        else
                            collectCandidates rule seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    UntilDateTime untilPosix ->
                        let
                            untilDate : Date
                            untilDate =
                                Date.fromPosix Time.utc untilPosix
                        in
                        if Date.compare c untilDate == GT then
                            List.reverse acc

                        else
                            collectCandidates rule seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest

                    Forever ->
                        if Date.toRataDie c > rangeEndRD then
                            List.reverse acc

                        else
                            collectCandidates rule seed rangeEndRD stepIndex (emittedCount + 1) (c :: acc) rest


isCountLimited : RecurrenceEnd -> Bool
isCountLimited end =
    case end of
        Count _ ->
            True

        _ ->
            False


advanceByInterval : Frequency -> Int -> Date -> Date
advanceByInterval freq n seed =
    case freq of
        Daily ->
            Date.add Date.Days n seed

        Weekly ->
            Date.add Date.Weeks n seed

        Monthly ->
            Date.add Date.Months n seed

        Yearly ->
            Date.add Date.Years n seed


expandWithinPeriod : RecurrenceRule -> Date -> Date -> List Date
expandWithinPeriod rule seed intervalDate =
    case rule.frequency of
        Daily ->
            [ intervalDate ]
                |> filterByMonth rule.byMonth
                |> filterByMonthDay rule.byMonthDay
                |> filterByDay rule.byDay

        Weekly ->
            if List.isEmpty rule.byDay then
                [ intervalDate ]
                    |> filterByMonth rule.byMonth

            else
                expandWeekByDay rule.weekStart rule.byDay intervalDate
                    |> filterByMonth rule.byMonth

        Monthly ->
            expandMonthly rule intervalDate

        Yearly ->
            expandYearly rule seed intervalDate


expandWeekByDay : Time.Weekday -> List DaySpec -> Date -> List Date
expandWeekByDay weekStart byDay date =
    let
        weekStartDate : Date
        weekStartDate =
            Date.floor (weekdayToInterval weekStart) date
    in
    byDay
        |> List.filterMap
            (\spec ->
                let
                    target : Date
                    target =
                        Date.ceiling (weekdayToInterval spec.weekday) weekStartDate
                in
                -- Ensure the target is within the same week
                if Date.toRataDie target - Date.toRataDie weekStartDate < 7 then
                    Just target

                else
                    Nothing
            )
        |> List.sortBy Date.toRataDie


expandMonthly : RecurrenceRule -> Date -> List Date
expandMonthly rule intervalDate =
    let
        year : Int
        year =
            Date.year intervalDate

        month : Time.Month
        month =
            Date.month intervalDate

        baseDates : List Date
        baseDates =
            if not (List.isEmpty rule.byMonthDay) then
                expandByMonthDay year month rule.byMonthDay

            else if not (List.isEmpty rule.byDay) then
                expandByDayInMonth year month rule.byDay

            else
                [ intervalDate ]
    in
    baseDates
        |> filterByMonth rule.byMonth
        |> List.sortBy Date.toRataDie


expandYearly : RecurrenceRule -> Date -> Date -> List Date
expandYearly rule seed intervalDate =
    let
        year : Int
        year =
            Date.year intervalDate

        months : List Time.Month
        months =
            if List.isEmpty rule.byMonth then
                [ Date.month seed ]

            else
                List.filterMap intToMonth_ rule.byMonth

        baseDates : List Date
        baseDates =
            if not (List.isEmpty rule.byMonthDay) then
                months
                    |> List.concatMap (\m -> expandByMonthDay year m rule.byMonthDay)

            else if not (List.isEmpty rule.byDay) then
                months
                    |> List.concatMap (\m -> expandByDayInMonth year m rule.byDay)

            else
                months
                    |> List.map (\m -> Date.fromCalendarDate year m (Date.day seed))
    in
    baseDates
        |> List.sortBy Date.toRataDie


expandByMonthDay : Int -> Time.Month -> List Int -> List Date
expandByMonthDay year month days =
    let
        maxDay : Int
        maxDay =
            daysInMonth year month
    in
    days
        |> List.filterMap
            (\d ->
                let
                    actualDay : Int
                    actualDay =
                        if d < 0 then
                            maxDay + d + 1

                        else
                            d
                in
                if actualDay >= 1 && actualDay <= maxDay then
                    Just (Date.fromCalendarDate year month actualDay)

                else
                    Nothing
            )


expandByDayInMonth : Int -> Time.Month -> List DaySpec -> List Date
expandByDayInMonth year month specs =
    specs
        |> List.concatMap (resolveDaySpecInMonth year month)
        |> List.sortBy Date.toRataDie


resolveDaySpecInMonth : Int -> Time.Month -> DaySpec -> List Date
resolveDaySpecInMonth year month spec =
    let
        interval : Date.Interval
        interval =
            weekdayToInterval spec.weekday
    in
    case spec.ordinal of
        Just n ->
            if n > 0 then
                let
                    firstOfMonth : Date
                    firstOfMonth =
                        Date.fromCalendarDate year month 1

                    firstMatch : Date
                    firstMatch =
                        Date.ceiling interval firstOfMonth

                    result : Date
                    result =
                        Date.add Date.Weeks (n - 1) firstMatch
                in
                -- Verify still in the same month
                if Date.month result == month then
                    [ result ]

                else
                    []

            else
                let
                    maxDay : Int
                    maxDay =
                        daysInMonth year month

                    lastOfMonth : Date
                    lastOfMonth =
                        Date.fromCalendarDate year month maxDay

                    lastMatch : Date
                    lastMatch =
                        Date.floor interval lastOfMonth

                    result : Date
                    result =
                        Date.add Date.Weeks (n + 1) lastMatch
                in
                if Date.month result == month then
                    [ result ]

                else
                    []

        Nothing ->
            -- All matching weekdays in the month
            let
                firstOfMonth : Date
                firstOfMonth =
                    Date.fromCalendarDate year month 1

                firstMatch : Date
                firstMatch =
                    Date.ceiling interval firstOfMonth
            in
            allWeekdaysInMonth month firstMatch []


allWeekdaysInMonth : Time.Month -> Date -> List Date -> List Date
allWeekdaysInMonth month current acc =
    if Date.month current /= month then
        List.reverse acc

    else
        allWeekdaysInMonth month (Date.add Date.Weeks 1 current) (current :: acc)


filterByMonth : List Int -> List Date -> List Date
filterByMonth byMonth dates =
    if List.isEmpty byMonth then
        dates

    else
        List.filter (\d -> List.member (monthToInt (Date.month d)) byMonth) dates


filterByMonthDay : List Int -> List Date -> List Date
filterByMonthDay byMonthDay dates =
    if List.isEmpty byMonthDay then
        dates

    else
        List.filter (\d -> List.member (Date.day d) byMonthDay) dates


filterByDay : List DaySpec -> List Date -> List Date
filterByDay byDay dates =
    if List.isEmpty byDay then
        dates

    else
        let
            weekdays : List Time.Weekday
            weekdays =
                List.map .weekday byDay
        in
        List.filter (\d -> List.member (Date.weekday d) weekdays) dates


applyBySetPos : List Int -> List Date -> List Date
applyBySetPos bySetPos dates =
    if List.isEmpty bySetPos then
        dates

    else
        let
            len : Int
            len =
                List.length dates

            indexed : List ( Int, Date )
            indexed =
                List.indexedMap Tuple.pair dates
        in
        bySetPos
            |> List.filterMap
                (\pos ->
                    let
                        idx : Int
                        idx =
                            if pos > 0 then
                                pos - 1

                            else
                                len + pos
                    in
                    indexed
                        |> List.filter (\( i, _ ) -> i == idx)
                        |> List.head
                        |> Maybe.map Tuple.second
                )
            |> List.sortBy Date.toRataDie


filterExclusions : Parser.Event -> List Date -> List Date
filterExclusions event dates =
    if List.isEmpty event.exclusions then
        dates

    else
        let
            exclusionRDs : List Int
            exclusionRDs =
                event.exclusions
                    |> List.map (\posix -> Date.toRataDie (Date.fromPosix Time.utc posix))
        in
        List.filter (\d -> not (List.member (Date.toRataDie d) exclusionRDs)) dates


startDate : Parser.EventTime -> Date
startDate eventTime =
    case eventTime of
        Parser.AllDay { start } ->
            start

        Parser.WithTime { start } ->
            Date.fromPosix Time.utc start.posix

        Parser.FloatingTime { start } ->
            Date.fromCalendarDate start.year (intToMonth start.month) start.day


shiftTime : Parser.EventTime -> Date -> Date -> Parser.EventTime
shiftTime originalTime originalDate newDate =
    let
        dayDelta : Int
        dayDelta =
            Date.toRataDie newDate - Date.toRataDie originalDate
    in
    case originalTime of
        Parser.AllDay { start, end } ->
            Parser.AllDay
                { start = Date.add Date.Days dayDelta start
                , end = Maybe.map (Date.add Date.Days dayDelta) end
                }

        Parser.WithTime { start, end } ->
            let
                deltaMs : Int
                deltaMs =
                    dayDelta * 86400000
            in
            Parser.WithTime
                { start = { start | posix = Time.millisToPosix (Time.posixToMillis start.posix + deltaMs) }
                , end =
                    Maybe.map
                        (\e -> { e | posix = Time.millisToPosix (Time.posixToMillis e.posix + deltaMs) })
                        end
                }

        Parser.FloatingTime { start, end } ->
            let
                newStart : Parser.LocalDateTime
                newStart =
                    shiftLocalDateTime dayDelta start

                newEnd : Maybe Parser.LocalDateTime
                newEnd =
                    Maybe.map (shiftLocalDateTime dayDelta) end
            in
            Parser.FloatingTime { start = newStart, end = newEnd }


shiftLocalDateTime : Int -> Parser.LocalDateTime -> Parser.LocalDateTime
shiftLocalDateTime dayDelta ldt =
    let
        originalDate : Date
        originalDate =
            Date.fromCalendarDate ldt.year (intToMonth ldt.month) ldt.day

        newDate : Date
        newDate =
            Date.add Date.Days dayDelta originalDate
    in
    { year = Date.year newDate
    , month = monthToInt (Date.month newDate)
    , day = Date.day newDate
    , hour = ldt.hour
    , minute = ldt.minute
    , second = ldt.second
    }


dedup : List Occurrence -> List Occurrence
dedup occurrences =
    dedupHelp occurrences -999999 []


dedupHelp : List Occurrence -> Int -> List Occurrence -> List Occurrence
dedupHelp remaining lastRD acc =
    case remaining of
        [] ->
            List.reverse acc

        occ :: rest ->
            let
                rd : Int
                rd =
                    Date.toRataDie (startDate occ.time)
            in
            if rd == lastRD then
                dedupHelp rest lastRD acc

            else
                dedupHelp rest rd (occ :: acc)


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


intToMonth : Int -> Time.Month
intToMonth m =
    case m of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec


intToMonth_ : Int -> Maybe Time.Month
intToMonth_ m =
    if m >= 1 && m <= 12 then
        Just (intToMonth m)

    else
        Nothing


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


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


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)
