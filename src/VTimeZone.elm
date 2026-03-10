module VTimeZone exposing (TransitionRule, ZoneDefinition, ZoneTransition, parseFromContentLines, parseOffset, parseTransitionRule, resolve, transitionDate)

{-| Internal module for VTIMEZONE parsing and timezone resolution.
-}

import ContentLine exposing (ContentLine)
import Date
import DateHelpers
import Time
import ValueParser


{-| A yearly transition rule extracted from a VTIMEZONE RRULE.
-}
type alias TransitionRule =
    { byMonth : Int
    , weekdayOrdinal : Int
    , weekday : Time.Weekday
    }


type ZoneDefinition
    = ZoneDefinition (List Observance)


{-| A concrete transition into an observance.
-}
type alias ZoneTransition =
    { localDateTime : LocalDateTime
    , offsetFrom : Int
    , offsetTo : Int
    }


type alias LocalDateTime =
    { year : Int
    , month : Time.Month
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


type alias Observance =
    { offsetFrom : Int
    , offsetTo : Int
    , dtstart : LocalDateTime
    , recurrence : Maybe TransitionPattern
    , rdates : List LocalDateTime
    }


type alias TransitionPattern =
    { rule : TransitionRule
    , until : Maybe Time.Posix
    }


{-| Parse an iCal UTC offset string like "-0500", "+0530", or "+013045"
into seconds from UTC.
-}
parseOffset : String -> Result String Int
parseOffset input =
    let
        length : Int
        length =
            String.length input

        hasValidLength : Bool
        hasValidLength =
            length == 5 || length == 7

        sign : String
        sign =
            String.left 1 input

        hourStr : String
        hourStr =
            String.slice 1 3 input

        minuteStr : String
        minuteStr =
            String.slice 3 5 input

        secondStr : String
        secondStr =
            if length == 7 then
                String.slice 5 7 input

            else
                "00"
    in
    if not hasValidLength then
        Err ("Invalid UTC offset: " ++ input)

    else
        case ( String.toInt hourStr, String.toInt minuteStr, String.toInt secondStr ) of
            ( Just hours, Just minutes, Just seconds ) ->
                case sign of
                    "+" ->
                        if isValidOffsetParts hours minutes seconds then
                            Ok (hours * 3600 + minutes * 60 + seconds)

                        else
                            Err ("Invalid UTC offset: " ++ input)

                    "-" ->
                        if (hours == 0 && minutes == 0 && seconds == 0) || not (isValidOffsetParts hours minutes seconds) then
                            Err ("Invalid UTC offset: " ++ input)

                        else
                            Ok (negate (hours * 3600 + minutes * 60 + seconds))

                    _ ->
                        Err ("Invalid UTC offset: " ++ input)

            _ ->
                Err ("Invalid UTC offset: " ++ input)


isValidOffsetParts : Int -> Int -> Int -> Bool
isValidOffsetParts hours minutes seconds =
    hours
        >= 0
        && hours
        <= 23
        && minutes
        >= 0
        && minutes
        <= 59
        && seconds
        >= 0
        && seconds
        <= 59


{-| Parse a VTIMEZONE RRULE like "FREQ=YEARLY;BYMONTH=3;BYDAY=2SU".
Only supports the subset used in VTIMEZONE definitions.
-}
parseTransitionRule : String -> Result String TransitionRule
parseTransitionRule input =
    parseTransitionPattern input
        |> Result.map .rule


parseTransitionPattern : String -> Result String TransitionPattern
parseTransitionPattern input =
    let
        lookup : String -> Maybe String
        lookup key =
            parsePairs input
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
                    parseTransitionUntil (lookup "UNTIL")
                        |> Result.map
                            (\until ->
                                { rule = { byMonth = month, weekdayOrdinal = ordinal, weekday = weekday }
                                , until = until
                                }
                            )

                _ ->
                    Err ("Invalid VTIMEZONE RRULE: " ++ input)

        _ ->
            Err ("Invalid VTIMEZONE RRULE: " ++ input)


parseTransitionUntil : Maybe String -> Result String (Maybe Time.Posix)
parseTransitionUntil maybeUntil =
    case maybeUntil of
        Nothing ->
            Ok Nothing

        Just untilValue ->
            ValueParser.parseDateTime untilValue
                |> Result.andThen
                    (\parts ->
                        if parts.isUtc then
                            Ok (Just (dateTimePartsToPosix parts))

                        else
                            Err ("VTIMEZONE UNTIL must be UTC: " ++ untilValue)
                    )


parsePairs : String -> List ( String, String )
parsePairs input =
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
E.g., "2nd Sunday of March 2024" -> March 10, 2024.
-}
transitionDate : Int -> TransitionRule -> Date.Date
transitionDate year rule =
    let
        month : Time.Month
        month =
            Date.numberToMonth rule.byMonth

        interval : Date.Interval
        interval =
            DateHelpers.weekdayToInterval rule.weekday
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
        let
            lastOfMonth : Date.Date
            lastOfMonth =
                Date.fromCalendarDate year month (DateHelpers.daysInMonth year month)

            lastMatch : Date.Date
            lastMatch =
                Date.floor interval lastOfMonth
        in
        Date.add Date.Weeks (rule.weekdayOrdinal + 1) lastMatch


{-| Parse a VTIMEZONE component from content lines (after BEGIN:VTIMEZONE).
Returns the TZID, the ZoneDefinition, and remaining unparsed lines.
-}
parseFromContentLines : List ContentLine -> Result String ( String, ZoneDefinition, List ContentLine )
parseFromContentLines lines =
    parseVTimeZoneBody lines Nothing []


parseVTimeZoneBody :
    List ContentLine
    -> Maybe String
    -> List Observance
    -> Result String ( String, ZoneDefinition, List ContentLine )
parseVTimeZoneBody lines maybeTzid observances =
    case lines of
        [] ->
            Err "Unexpected end of input, expected END:VTIMEZONE"

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == "VTIMEZONE" then
                case maybeTzid of
                    Nothing ->
                        Err "VTIMEZONE missing TZID"

                    Just tzid ->
                        if List.isEmpty observances then
                            Err "VTIMEZONE must contain at least one STANDARD or DAYLIGHT observance"

                        else
                            Ok ( tzid, ZoneDefinition (List.reverse observances), rest )

            else if line.name == "TZID" then
                parseVTimeZoneBody rest (Just line.value) observances

            else if line.name == "BEGIN" && String.toUpper line.value == "STANDARD" then
                case parseObservance "STANDARD" rest of
                    Ok ( observance, remaining ) ->
                        parseVTimeZoneBody remaining maybeTzid (observance :: observances)

                    Err err ->
                        Err err

            else if line.name == "BEGIN" && String.toUpper line.value == "DAYLIGHT" then
                case parseObservance "DAYLIGHT" rest of
                    Ok ( observance, remaining ) ->
                        parseVTimeZoneBody remaining maybeTzid (observance :: observances)

                    Err err ->
                        Err err

            else
                parseVTimeZoneBody rest maybeTzid observances


type alias ObservanceAccum =
    { offsetFrom : Maybe Int
    , offsetTo : Maybe Int
    , rrule : Maybe TransitionPattern
    , dtstart : Maybe LocalDateTime
    , rdates : List LocalDateTime
    }


parseObservance : String -> List ContentLine -> Result String ( Observance, List ContentLine )
parseObservance componentName lines =
    parseObservanceBody componentName
        lines
        { offsetFrom = Nothing
        , offsetTo = Nothing
        , rrule = Nothing
        , dtstart = Nothing
        , rdates = []
        }


parseObservanceBody : String -> List ContentLine -> ObservanceAccum -> Result String ( Observance, List ContentLine )
parseObservanceBody componentName lines acc =
    case lines of
        [] ->
            Err ("Unexpected end of input, expected END:" ++ componentName)

        line :: rest ->
            if line.name == "END" && String.toUpper line.value == componentName then
                case ( acc.offsetFrom, acc.offsetTo, acc.dtstart ) of
                    ( Just offsetFrom, Just offsetTo, Just dtstart ) ->
                        Ok
                            ( { offsetFrom = offsetFrom
                              , offsetTo = offsetTo
                              , dtstart = dtstart
                              , recurrence = acc.rrule
                              , rdates = List.reverse acc.rdates
                              }
                            , rest
                            )

                    ( Nothing, _, _ ) ->
                        Err ("Observance missing TZOFFSETFROM in " ++ componentName)

                    ( _, Nothing, _ ) ->
                        Err ("Observance missing TZOFFSETTO in " ++ componentName)

                    ( _, _, Nothing ) ->
                        Err ("Observance missing DTSTART in " ++ componentName)

            else if line.name == "TZOFFSETFROM" then
                case parseOffset line.value of
                    Ok offset ->
                        parseObservanceBody componentName rest { acc | offsetFrom = Just offset }

                    Err err ->
                        Err err

            else if line.name == "TZOFFSETTO" then
                case parseOffset line.value of
                    Ok offset ->
                        parseObservanceBody componentName rest { acc | offsetTo = Just offset }

                    Err err ->
                        Err err

            else if line.name == "RRULE" then
                case parseTransitionPattern line.value of
                    Ok pattern ->
                        parseObservanceBody componentName rest { acc | rrule = Just pattern }

                    Err err ->
                        Err err

            else if line.name == "DTSTART" then
                case parseLocalDateTime line.value of
                    Ok dtstart ->
                        parseObservanceBody componentName rest { acc | dtstart = Just dtstart }

                    Err err ->
                        Err err

            else if line.name == "RDATE" then
                case parseRDates line.value of
                    Ok rdates ->
                        parseObservanceBody componentName rest { acc | rdates = List.reverse rdates ++ acc.rdates }

                    Err err ->
                        Err err

            else
                parseObservanceBody componentName rest acc


parseLocalDateTime : String -> Result String LocalDateTime
parseLocalDateTime value =
    ValueParser.parseDateTime value
        |> Result.andThen
            (\parts ->
                if parts.isUtc then
                    Err ("VTIMEZONE local date-time must not be UTC: " ++ value)

                else
                    Ok
                        { year = parts.year
                        , month = Date.numberToMonth parts.month
                        , day = parts.day
                        , hour = parts.hour
                        , minute = parts.minute
                        , second = normalizeSecond parts.second
                        }
            )


parseRDates : String -> Result String (List LocalDateTime)
parseRDates value =
    value
        |> String.split ","
        |> List.foldr
            (\part acc ->
                case ( parseLocalDateTime part, acc ) of
                    ( Ok localDateTime, Ok localDateTimes ) ->
                        Ok (localDateTime :: localDateTimes)

                    ( Err err, _ ) ->
                        Err err

                    ( _, Err err ) ->
                        Err err
            )
            (Ok [])


{-| Resolve a local datetime to UTC Posix using VTIMEZONE data.
Per RFC 5545 Section 3.3.5, ambiguous times (during fall-back) resolve to the
first occurrence, and nonexistent times (during spring-forward) use the offset
before the gap.
-}
resolve :
    ZoneDefinition
    -> { year : Int, month : Time.Month, day : Int, hour : Int, minute : Int, second : Int }
    -> Result String Time.Posix
resolve (ZoneDefinition observances) dt =
    let
        transitions : List ZoneTransition
        transitions =
            observances
                |> List.concatMap (transitionsForYearRange (dt.year - 2) (dt.year + 1))
                |> List.sortBy transitionUtcSeconds

        localDateTime : LocalDateTime
        localDateTime =
            { year = dt.year
            , month = dt.month
            , day = dt.day
            , hour = dt.hour
            , minute = dt.minute
            , second = normalizeSecond dt.second
            }
    in
    case transitions of
        [] ->
            Err "VTIMEZONE has no usable transitions"

        firstTransition :: _ ->
            let
                offsetSeconds : Int
                offsetSeconds =
                    activeOffset firstTransition.offsetFrom transitions localDateTime

                utcSeconds : Int
                utcSeconds =
                    localDateTimeToPseudoSeconds localDateTime - offsetSeconds
            in
            Ok (Time.millisToPosix (utcSeconds * 1000))


transitionsForYearRange : Int -> Int -> Observance -> List ZoneTransition
transitionsForYearRange startYear endYear observance =
    let
        dtstartTransition : List ZoneTransition
        dtstartTransition =
            if observance.dtstart.year >= startYear && observance.dtstart.year <= endYear then
                [ transitionFromLocal observance.offsetFrom observance.offsetTo observance.dtstart ]

            else
                []

        recurringTransitions : List ZoneTransition
        recurringTransitions =
            case observance.recurrence of
                Nothing ->
                    []

                Just pattern ->
                    List.range startYear endYear
                        |> List.filterMap (transitionForYear observance pattern)

        rdateTransitions : List ZoneTransition
        rdateTransitions =
            observance.rdates
                |> List.filter (\rdate -> rdate.year >= startYear && rdate.year <= endYear)
                |> List.map (transitionFromLocal observance.offsetFrom observance.offsetTo)
    in
    dtstartTransition ++ recurringTransitions ++ rdateTransitions


transitionForYear : Observance -> TransitionPattern -> Int -> Maybe ZoneTransition
transitionForYear observance pattern year =
    if year < observance.dtstart.year then
        Nothing

    else
        let
            date : Date.Date
            date =
                transitionDate year pattern.rule

            localDateTime : LocalDateTime
            localDateTime =
                { year = year
                , month = Date.month date
                , day = Date.day date
                , hour = observance.dtstart.hour
                , minute = observance.dtstart.minute
                , second = observance.dtstart.second
                }

            transition : ZoneTransition
            transition =
                transitionFromLocal observance.offsetFrom observance.offsetTo localDateTime
        in
        if year == observance.dtstart.year && localDateTime /= observance.dtstart then
            Nothing

        else if isBeforeOrEqualUntil pattern.until transition then
            Just transition

        else
            Nothing


transitionFromLocal : Int -> Int -> LocalDateTime -> ZoneTransition
transitionFromLocal offsetFrom offsetTo localDateTime =
    { localDateTime = localDateTime
    , offsetFrom = offsetFrom
    , offsetTo = offsetTo
    }


isBeforeOrEqualUntil : Maybe Time.Posix -> ZoneTransition -> Bool
isBeforeOrEqualUntil maybeUntil transition =
    case maybeUntil of
        Nothing ->
            True

        Just untilPosix ->
            transitionUtcSeconds transition <= Time.posixToMillis untilPosix // 1000


activeOffset : Int -> List ZoneTransition -> LocalDateTime -> Int
activeOffset currentOffset transitions localDateTime =
    case transitions of
        [] ->
            currentOffset

        transition :: rest ->
            let
                targetLocalSeconds : Int
                targetLocalSeconds =
                    localDateTimeToPseudoSeconds localDateTime

                transitionLocalSeconds : Int
                transitionLocalSeconds =
                    localDateTimeToPseudoSeconds transition.localDateTime

                gapSeconds : Int
                gapSeconds =
                    transition.offsetTo - transition.offsetFrom
            in
            if targetLocalSeconds < transitionLocalSeconds then
                currentOffset

            else if gapSeconds > 0 && targetLocalSeconds < transitionLocalSeconds + gapSeconds then
                transition.offsetFrom

            else
                activeOffset transition.offsetTo rest localDateTime


transitionUtcSeconds : ZoneTransition -> Int
transitionUtcSeconds transition =
    localDateTimeToPseudoSeconds transition.localDateTime - transition.offsetFrom


localDateTimeToPseudoSeconds : LocalDateTime -> Int
localDateTimeToPseudoSeconds localDateTime =
    let
        date : Date.Date
        date =
            Date.fromCalendarDate localDateTime.year localDateTime.month localDateTime.day
    in
    (Date.toRataDie date - 719163)
        * 86400
        + localDateTime.hour
        * 3600
        + localDateTime.minute
        * 60
        + localDateTime.second


dateTimePartsToPosix : ValueParser.DateTimeParts -> Time.Posix
dateTimePartsToPosix parts =
    let
        date : Date.Date
        date =
            Date.fromCalendarDate parts.year (Date.numberToMonth parts.month) parts.day

        totalSeconds : Int
        totalSeconds =
            (Date.toRataDie date - 719163)
                * 86400
                + parts.hour
                * 3600
                + parts.minute
                * 60
                + normalizeSecond parts.second
    in
    Time.millisToPosix (totalSeconds * 1000)


normalizeSecond : Int -> Int
normalizeSecond second =
    if second == 60 then
        59

    else
        second
