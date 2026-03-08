module VTimeZone exposing (TransitionRule, ZoneDefinition, ZoneTransition, parseFromContentLines, parseOffset, parseTransitionRule, resolve, transitionDate)

{-| Internal module for VTIMEZONE parsing and timezone resolution.
-}

import ContentLine exposing (ContentLine)
import Date
import Time
import ValueParser


{-| A yearly transition rule extracted from a VTIMEZONE RRULE.
-}
type alias TransitionRule =
    { byMonth : Int
    , weekdayOrdinal : Int
    , weekday : Time.Weekday
    }


{-| Parse an iCal UTC offset string like "-0500" or "+0530" into minutes from UTC.
-}
parseOffset : String -> Result String Int
parseOffset input =
    if String.length input /= 5 then
        Err ("Invalid UTC offset: " ++ input)

    else
        let
            sign : String
            sign =
                String.left 1 input

            hourStr : String
            hourStr =
                String.slice 1 3 input

            minStr : String
            minStr =
                String.slice 3 5 input
        in
        case ( sign, String.toInt hourStr, String.toInt minStr ) of
            ( "+", Just hours, Just mins ) ->
                Ok (hours * 60 + mins)

            ( "-", Just hours, Just mins ) ->
                Ok (negate (hours * 60 + mins))

            _ ->
                Err ("Invalid UTC offset: " ++ input)


{-| Parse a VTIMEZONE RRULE like "FREQ=YEARLY;BYMONTH=3;BYDAY=2SU".
Only supports the subset used in VTIMEZONE definitions.
-}
parseTransitionRule : String -> Result String TransitionRule
parseTransitionRule input =
    let
        parts : List ( String, String )
        parts =
            input
                |> String.split ";"
                |> List.filterMap
                    (\part ->
                        case String.split "=" part of
                            [ key, value ] ->
                                Just ( String.toUpper key, value )

                            _ ->
                                Nothing
                    )

        lookup : String -> Maybe String
        lookup key =
            parts
                |> List.filterMap
                    (\( k, v ) ->
                        if k == key then
                            Just v

                        else
                            Nothing
                    )
                |> List.head
    in
    case ( lookup "FREQ", lookup "BYMONTH", lookup "BYDAY" ) of
        ( Just "YEARLY", Just monthStr, Just dayStr ) ->
            case ( String.toInt monthStr, parseByDay dayStr ) of
                ( Just month, Ok ( ordinal, weekday ) ) ->
                    Ok { byMonth = month, weekdayOrdinal = ordinal, weekday = weekday }

                _ ->
                    Err ("Invalid VTIMEZONE RRULE: " ++ input)

        _ ->
            Err ("Invalid VTIMEZONE RRULE: " ++ input)


parseByDay : String -> Result String ( Int, Time.Weekday )
parseByDay input =
    let
        daySuffix : String
        daySuffix =
            String.right 2 input

        ordinalStr : String
        ordinalStr =
            String.dropRight 2 input
    in
    case ( String.toInt ordinalStr, parseWeekday daySuffix ) of
        ( Just ordinal, Just weekday ) ->
            Ok ( ordinal, weekday )

        _ ->
            Err ("Invalid BYDAY: " ++ input)


parseWeekday : String -> Maybe Time.Weekday
parseWeekday str =
    case String.toUpper str of
        "MO" ->
            Just Time.Mon

        "TU" ->
            Just Time.Tue

        "WE" ->
            Just Time.Wed

        "TH" ->
            Just Time.Thu

        "FR" ->
            Just Time.Fri

        "SA" ->
            Just Time.Sat

        "SU" ->
            Just Time.Sun

        _ ->
            Nothing


{-| Compute the date of a yearly transition in a given year.
E.g., "2nd Sunday of March 2024" → March 10, 2024.
-}
transitionDate : Int -> TransitionRule -> Date.Date
transitionDate year rule =
    let
        month : Time.Month
        month =
            intToMonth rule.byMonth

        interval : Date.Interval
        interval =
            weekdayToInterval rule.weekday
    in
    if rule.weekdayOrdinal > 0 then
        let
            firstOfMonth : Date.Date
            firstOfMonth =
                Date.fromCalendarDate year month 1

            firstMatch : Date.Date
            firstMatch =
                Date.ceiling interval firstOfMonth
        in
        Date.add Date.Weeks (rule.weekdayOrdinal - 1) firstMatch

    else
        -- Negative ordinal: count from end of month
        let
            lastOfMonth : Date.Date
            lastOfMonth =
                Date.fromCalendarDate year month (daysInMonth year month)

            lastMatch : Date.Date
            lastMatch =
                Date.floor interval lastOfMonth
        in
        Date.add Date.Weeks (rule.weekdayOrdinal + 1) lastMatch


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
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))


{-| Parse a VTIMEZONE component from content lines (after BEGIN:VTIMEZONE).
Returns the TZID, the ZoneDefinition, and remaining unparsed lines.
-}
parseFromContentLines : List ContentLine -> Result String ( String, ZoneDefinition, List ContentLine )
parseFromContentLines lines =
    parseVTimeZoneBody lines Nothing Nothing Nothing
        |> Result.andThen
            (\( tzid, observances, remaining ) ->
                case tzid of
                    Nothing ->
                        Err "VTIMEZONE missing TZID"

                    Just id ->
                        Ok ( id, buildZoneDefinition observances, remaining )
            )


type alias ObservanceParts =
    { offsetTo : Int
    , transition : Maybe ZoneTransition
    , isStandard : Bool
    }


parseVTimeZoneBody :
    List ContentLine
    -> Maybe String
    -> Maybe (List ObservanceParts)
    -> Maybe (List ObservanceParts)
    -> Result String ( Maybe String, List ObservanceParts, List ContentLine )
parseVTimeZoneBody lines maybeTzid standardParts daylightParts =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VTIMEZONE"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VTIMEZONE" then
                Ok
                    ( maybeTzid
                    , Maybe.withDefault [] standardParts ++ Maybe.withDefault [] daylightParts
                    , rest
                    )

            else if line.name == "TZID" then
                parseVTimeZoneBody rest (Just line.value) standardParts daylightParts

            else if line.name == "BEGIN" && String.toUpper line.value == "STANDARD" then
                case parseObservance rest True of
                    Ok ( obs, remaining ) ->
                        let
                            current : List ObservanceParts
                            current =
                                Maybe.withDefault [] standardParts
                        in
                        parseVTimeZoneBody remaining maybeTzid (Just (obs :: current)) daylightParts

                    Err err ->
                        Err err

            else if line.name == "BEGIN" && String.toUpper line.value == "DAYLIGHT" then
                case parseObservance rest False of
                    Ok ( obs, remaining ) ->
                        let
                            current : List ObservanceParts
                            current =
                                Maybe.withDefault [] daylightParts
                        in
                        parseVTimeZoneBody remaining maybeTzid standardParts (Just (obs :: current))

                    Err err ->
                        Err err

            else
                -- Skip unknown properties
                parseVTimeZoneBody rest maybeTzid standardParts daylightParts


parseObservance : List ContentLine -> Bool -> Result String ( ObservanceParts, List ContentLine )
parseObservance lines isStandard =
    parseObservanceBody lines
        { offsetTo = Nothing
        , rrule = Nothing
        , dtstart = Nothing
        , isStandard = isStandard
        }


type alias ObservanceAccum =
    { offsetTo : Maybe Int
    , rrule : Maybe TransitionRule
    , dtstart : Maybe { hour : Int, minute : Int, second : Int }
    , isStandard : Bool
    }


parseObservanceBody : List ContentLine -> ObservanceAccum -> Result String ( ObservanceParts, List ContentLine )
parseObservanceBody lines acc =
    let
        expectedEnd : String
        expectedEnd =
            if acc.isStandard then
                "STANDARD"

            else
                "DAYLIGHT"
    in
    case lines of
        [] ->
            Err ("Unexpected end of input, expected END:" ++ expectedEnd)

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == expectedEnd then
                case acc.offsetTo of
                    Just offset ->
                        let
                            transition : Maybe ZoneTransition
                            transition =
                                case ( acc.rrule, acc.dtstart ) of
                                    ( Just rule, Just time ) ->
                                        Just { rule = rule, transitionTime = time }

                                    ( Just rule, Nothing ) ->
                                        Just { rule = rule, transitionTime = { hour = 0, minute = 0, second = 0 } }

                                    _ ->
                                        Nothing
                        in
                        Ok
                            ( { offsetTo = offset
                              , transition = transition
                              , isStandard = acc.isStandard
                              }
                            , rest
                            )

                    Nothing ->
                        Err ("Observance missing TZOFFSETTO in " ++ expectedEnd)

            else if line.name == "TZOFFSETTO" then
                case parseOffset line.value of
                    Ok offset ->
                        parseObservanceBody rest { acc | offsetTo = Just offset }

                    Err err ->
                        Err err

            else if line.name == "RRULE" then
                case parseTransitionRule line.value of
                    Ok rule ->
                        parseObservanceBody rest { acc | rrule = Just rule }

                    Err err ->
                        Err err

            else if line.name == "DTSTART" then
                case ValueParser.parseDateTime line.value of
                    Ok parts ->
                        parseObservanceBody rest
                            { acc
                                | dtstart =
                                    Just
                                        { hour = parts.hour
                                        , minute = parts.minute
                                        , second = parts.second
                                        }
                            }

                    Err _ ->
                        -- Skip invalid DTSTART
                        parseObservanceBody rest acc

            else
                parseObservanceBody rest acc


buildZoneDefinition : List ObservanceParts -> ZoneDefinition
buildZoneDefinition observances =
    let
        standard : Maybe ObservanceParts
        standard =
            observances
                |> List.filter .isStandard
                |> List.head

        daylight : Maybe ObservanceParts
        daylight =
            observances
                |> List.filter (\o -> not o.isStandard)
                |> List.head
    in
    { standardOffset = standard |> Maybe.map .offsetTo |> Maybe.withDefault 0
    , standardTransition = standard |> Maybe.andThen .transition
    , daylightOffset = daylight |> Maybe.map .offsetTo |> Maybe.withDefault 0
    , daylightTransition = daylight |> Maybe.andThen .transition
    }


{-| A parsed VTIMEZONE definition with standard and optional daylight observances.
-}
type alias ZoneDefinition =
    { standardOffset : Int
    , standardTransition : Maybe ZoneTransition
    , daylightOffset : Int
    , daylightTransition : Maybe ZoneTransition
    }


{-| A transition into an observance (standard or daylight).
-}
type alias ZoneTransition =
    { rule : TransitionRule
    , transitionTime : { hour : Int, minute : Int, second : Int }
    }


{-| Resolve a local datetime to UTC Posix using VTIMEZONE data.
Per RFC 5545 Section 3.3.5, ambiguous times (during fall-back) resolve to the
first occurrence (the pre-transition/daylight side).
-}
resolve :
    ZoneDefinition
    -> { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
    -> Result String Time.Posix
resolve zone dt =
    let
        offsetMinutes : Int
        offsetMinutes =
            activeOffset zone dt

        date : Date.Date
        date =
            Date.fromCalendarDate dt.year (intToMonth dt.month) dt.day

        daysSinceEpoch : Int
        daysSinceEpoch =
            Date.toRataDie date - 719163

        localSeconds : Int
        localSeconds =
            daysSinceEpoch * 86400 + dt.hour * 3600 + dt.minute * 60 + dt.second

        utcSeconds : Int
        utcSeconds =
            localSeconds - (offsetMinutes * 60)
    in
    Ok (Time.millisToPosix (utcSeconds * 1000))


{-| Determine which UTC offset is active for a given local datetime.
-}
activeOffset : ZoneDefinition -> { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int } -> Int
activeOffset zone dt =
    case ( zone.daylightTransition, zone.standardTransition ) of
        ( Just daylight, Just standard ) ->
            let
                daylightDate : Date.Date
                daylightDate =
                    transitionDate dt.year daylight.rule

                standardDate : Date.Date
                standardDate =
                    transitionDate dt.year standard.rule

                localDate : Date.Date
                localDate =
                    Date.fromCalendarDate dt.year (intToMonth dt.month) dt.day

                daylightRataDie : Int
                daylightRataDie =
                    Date.toRataDie daylightDate

                standardRataDie : Int
                standardRataDie =
                    Date.toRataDie standardDate

                localRataDie : Int
                localRataDie =
                    Date.toRataDie localDate

                localMinuteOfDay : Int
                localMinuteOfDay =
                    dt.hour * 60 + dt.minute

                daylightTransitionMinute : Int
                daylightTransitionMinute =
                    daylight.transitionTime.hour * 60 + daylight.transitionTime.minute

                standardTransitionMinute : Int
                standardTransitionMinute =
                    standard.transitionTime.hour * 60 + standard.transitionTime.minute

                isAfterDaylightTransition : Bool
                isAfterDaylightTransition =
                    (localRataDie > daylightRataDie)
                        || (localRataDie == daylightRataDie && localMinuteOfDay >= daylightTransitionMinute)

                isBeforeStandardTransition : Bool
                isBeforeStandardTransition =
                    (localRataDie < standardRataDie)
                        || (localRataDie == standardRataDie && localMinuteOfDay < standardTransitionMinute)
            in
            if isAfterDaylightTransition && isBeforeStandardTransition then
                zone.daylightOffset

            else
                zone.standardOffset

        _ ->
            -- No DST transitions, use standard offset
            zone.standardOffset
