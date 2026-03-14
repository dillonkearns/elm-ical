module ValueParser exposing (DateTimeParts, Duration, parseDate, parseDateTime, parseDuration, parseRecurrenceRule, unescapeText)

{-| Parsers for iCal value types: DATE, DATE-TIME, DURATION, RECUR, and TEXT unescaping.
-}

import Date
import DateHelpers
import Ical.Recurrence exposing (DaySpec(..), Frequency(..), RecurrenceEnd(..), RecurrenceRule)
import Time


{-| Intermediate parsed date-time parts. The caller decides whether to wrap
this as UTC, local, or with-TZID based on context.
-}
type alias DateTimeParts =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , isUtc : Bool
    }


{-| Parse an iCal DATE value like "20210318" into year/month/day.
-}
parseDate : String -> Result String { year : Int, month : Int, day : Int }
parseDate input =
    if String.length input /= 8 then
        Err ("Invalid DATE: " ++ input)

    else
        case
            ( String.toInt (String.left 4 input)
            , String.toInt (String.slice 4 6 input)
            , String.toInt (String.slice 6 8 input)
            )
        of
            ( Just year, Just month, Just day ) ->
                if isValidDate year month day then
                    Ok { year = year, month = month, day = day }

                else
                    Err ("Invalid DATE: " ++ input)

            _ ->
                Err ("Invalid DATE: " ++ input)


{-| Parse an iCal DATE-TIME value like "20210318T162044Z" or "20210318T162044".
Returns intermediate parts with an isUtc flag.
-}
parseDateTime : String -> Result String DateTimeParts
parseDateTime input =
    let
        hasZ : Bool
        hasZ =
            String.endsWith "Z" input

        base : String
        base =
            if hasZ then
                String.dropRight 1 input

            else
                input
    in
    if String.length base /= 15 then
        Err ("Invalid DATE-TIME: " ++ input)

    else
        case
            ( String.toInt (String.left 4 base)
            , String.toInt (String.slice 4 6 base)
            , String.toInt (String.slice 6 8 base)
            )
        of
            ( Just year, Just month, Just day ) ->
                if String.slice 8 9 base /= "T" then
                    Err ("Invalid DATE-TIME: " ++ input)

                else
                    case
                        ( String.toInt (String.slice 9 11 base)
                        , String.toInt (String.slice 11 13 base)
                        , String.toInt (String.slice 13 15 base)
                        )
                    of
                        ( Just hour, Just minute, Just second ) ->
                            if isValidDate year month day && isValidTime hour minute second then
                                Ok
                                    { year = year
                                    , month = month
                                    , day = day
                                    , hour = hour
                                    , minute = minute
                                    , second = second
                                    , isUtc = hasZ
                                    }

                            else
                                Err ("Invalid DATE-TIME: " ++ input)

                        _ ->
                            Err ("Invalid DATE-TIME: " ++ input)

            _ ->
                Err ("Invalid DATE-TIME: " ++ input)


{-| A parsed iCal DURATION value (RFC 5545 Section 3.3.6).
-}
type alias Duration =
    { weeks : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


{-| Parse an iCal DURATION value like "PT1H30M", "P1D", "P1W", "P1DT2H30M".
-}
parseDuration : String -> Result String Duration
parseDuration input =
    if String.startsWith "-" input then
        Err "Negative durations are not supported"

    else
        let
            stripped : String
            stripped =
                if String.startsWith "+" input then
                    String.dropLeft 1 input

                else
                    input
        in
        if not (String.startsWith "P" stripped) then
            Err ("Invalid DURATION: " ++ input)

        else
            let
                afterP : String
                afterP =
                    String.dropLeft 1 stripped
            in
            if String.contains "W" afterP then
                -- Week form: P<n>W
                if String.endsWith "W" afterP && isDigits (String.dropRight 1 afterP) then
                    case String.toInt (String.dropRight 1 afterP) of
                        Just w ->
                            Ok { weeks = w, days = 0, hours = 0, minutes = 0, seconds = 0 }

                        Nothing ->
                            Err ("Invalid DURATION: " ++ input)

                else
                    Err ("Invalid DURATION: " ++ input)

            else
                case String.split "T" afterP of
                    [ datePart, timePart ] ->
                        parseDurationDays datePart
                            |> Result.andThen
                                (\days ->
                                    parseDurationTime timePart
                                        |> Result.map
                                            (\time ->
                                                { weeks = 0
                                                , days = days
                                                , hours = time.hours
                                                , minutes = time.minutes
                                                , seconds = time.seconds
                                                }
                                            )
                                )

                    [ datePart ] ->
                        if String.isEmpty datePart then
                            Err ("Invalid DURATION: " ++ input)

                        else
                            parseDurationDays datePart
                                |> Result.map
                                    (\days ->
                                        { weeks = 0, days = days, hours = 0, minutes = 0, seconds = 0 }
                                    )

                    _ ->
                        Err ("Invalid DURATION: " ++ input)


parseDurationDays : String -> Result String Int
parseDurationDays part =
    if String.isEmpty part then
        Ok 0

    else if String.endsWith "D" part && isDigits (String.dropRight 1 part) then
        case String.toInt (String.dropRight 1 part) of
            Just days ->
                Ok days

            Nothing ->
                Err ("Invalid DURATION day part: " ++ part)

    else
        Err ("Invalid DURATION day part: " ++ part)


parseDurationTime : String -> Result String { hours : Int, minutes : Int, seconds : Int }
parseDurationTime part =
    if String.isEmpty part then
        Err "Empty time part in DURATION"

    else
        parseDurationTimeHelp part 0 { hours = 0, minutes = 0, seconds = 0 }


parseDurationTimeHelp :
    String
    -> Int
    -> { hours : Int, minutes : Int, seconds : Int }
    -> Result String { hours : Int, minutes : Int, seconds : Int }
parseDurationTimeHelp remaining lastUnitRank acc =
    if String.isEmpty remaining then
        Ok acc

    else
        let
            ( digits, rest ) =
                spanDigits remaining

            unit : String
            unit =
                String.left 1 rest

            maybeRank : Maybe Int
            maybeRank =
                case unit of
                    "H" ->
                        Just 1

                    "M" ->
                        Just 2

                    "S" ->
                        Just 3

                    _ ->
                        Nothing
        in
        case ( String.toInt digits, maybeRank ) of
            ( Just n, Just rank ) ->
                if String.isEmpty digits || rank < lastUnitRank then
                    Err ("Invalid DURATION time part: " ++ remaining)

                else
                    case unit of
                        "H" ->
                            parseDurationTimeHelp (String.dropLeft 1 rest) rank { acc | hours = n }

                        "M" ->
                            parseDurationTimeHelp (String.dropLeft 1 rest) rank { acc | minutes = n }

                        "S" ->
                            parseDurationTimeHelp (String.dropLeft 1 rest) rank { acc | seconds = n }

                        _ ->
                            Err ("Invalid DURATION time part: " ++ remaining)

            _ ->
                Err ("Invalid DURATION time part: " ++ remaining)


spanDigits : String -> ( String, String )
spanDigits input =
    spanDigitsHelp (String.toList input) []


spanDigitsHelp : List Char -> List Char -> ( String, String )
spanDigitsHelp remaining acc =
    case remaining of
        c :: rest ->
            if Char.isDigit c then
                spanDigitsHelp rest (c :: acc)

            else
                ( String.fromList (List.reverse acc), String.fromList remaining )

        [] ->
            ( String.fromList (List.reverse acc), "" )


isDigits : String -> Bool
isDigits input =
    not (String.isEmpty input) && String.all Char.isDigit input


isValidDate : Int -> Int -> Int -> Bool
isValidDate year month day =
    case daysInMonth year month of
        Just maxDay ->
            day >= 1 && day <= maxDay

        Nothing ->
            False


isValidTime : Int -> Int -> Int -> Bool
isValidTime hour minute second =
    hour
        >= 0
        && hour
        <= 23
        && minute
        >= 0
        && minute
        <= 59
        && second
        >= 0
        && second
        <= 60


daysInMonth : Int -> Int -> Maybe Int
daysInMonth year month =
    case month of
        1 ->
            Just 31

        2 ->
            Just
                (if DateHelpers.isLeapYear year then
                    29

                 else
                    28
                )

        3 ->
            Just 31

        4 ->
            Just 30

        5 ->
            Just 31

        6 ->
            Just 30

        7 ->
            Just 31

        8 ->
            Just 31

        9 ->
            Just 30

        10 ->
            Just 31

        11 ->
            Just 30

        12 ->
            Just 31

        _ ->
            Nothing


{-| Unescape iCal TEXT values per RFC 5545 Section 3.3.11.
-}
unescapeText : String -> String
unescapeText input =
    unescapeHelp (String.toList input) []
        |> List.reverse
        |> String.fromList


unescapeHelp : List Char -> List Char -> List Char
unescapeHelp remaining acc =
    case remaining of
        [] ->
            acc

        '\\' :: 'n' :: rest ->
            unescapeHelp rest ('\n' :: acc)

        '\\' :: 'N' :: rest ->
            unescapeHelp rest ('\n' :: acc)

        '\\' :: '\\' :: rest ->
            unescapeHelp rest ('\\' :: acc)

        '\\' :: ',' :: rest ->
            unescapeHelp rest (',' :: acc)

        '\\' :: ';' :: rest ->
            unescapeHelp rest (';' :: acc)

        '\\' :: c :: rest ->
            unescapeHelp rest (c :: acc)

        c :: rest ->
            unescapeHelp rest (c :: acc)


{-| Parse an iCal RECUR value (RFC 5545 Section 3.3.10) into a RecurrenceRule.
-}
parseRecurrenceRule : String -> Result String RecurrenceRule
parseRecurrenceRule input =
    let
        parts : List ( String, String )
        parts =
            String.split ";" input
                |> List.filterMap
                    (\part ->
                        case String.split "=" part of
                            [ k, v ] ->
                                Just ( String.toUpper k, v )

                            _ ->
                                Nothing
                    )

        getParam : String -> Maybe String
        getParam key =
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
    case getParam "FREQ" of
        Nothing ->
            Err ("Invalid RRULE: missing FREQ in " ++ input)

        Just freqStr ->
            case parseFrequency freqStr of
                Nothing ->
                    Err ("Invalid RRULE: unknown FREQ=" ++ freqStr)

                Just freqTag ->
                    parseInterval (getParam "INTERVAL")
                        |> Result.andThen
                            (\interval ->
                                parseWeekStart (getParam "WKST")
                                    |> Result.andThen
                                        (\weekStart ->
                                            let
                                                frequency : Frequency
                                                frequency =
                                                    buildFrequency freqTag interval weekStart
                                            in
                                            parseRecurrenceEnd (getParam "COUNT") (getParam "UNTIL")
                                                |> Result.andThen
                                                    (\end ->
                                                        parseDaySpecs (getParam "BYDAY")
                                                            |> Result.andThen
                                                                (\byDay ->
                                                                    parseByParts getParam
                                                                        |> Result.map
                                                                            (\bp ->
                                                                                { frequency = frequency
                                                                                , end = end
                                                                                , byDay = byDay
                                                                                , byMonthDay = bp.byMonthDay
                                                                                , byMonth = bp.byMonth
                                                                                , bySetPos = bp.bySetPos
                                                                                , byHour = bp.byHour
                                                                                , byMinute = bp.byMinute
                                                                                , bySecond = bp.bySecond
                                                                                , byYearDay = bp.byYearDay
                                                                                , byWeekNo = bp.byWeekNo
                                                                                }
                                                                            )
                                                                )
                                                    )
                                        )
                            )


type FreqTag
    = SecondlyTag
    | MinutelyTag
    | HourlyTag
    | DailyTag
    | WeeklyTag
    | MonthlyTag
    | YearlyTag


parseFrequency : String -> Maybe FreqTag
parseFrequency str =
    case String.toUpper str of
        "SECONDLY" ->
            Just SecondlyTag

        "MINUTELY" ->
            Just MinutelyTag

        "HOURLY" ->
            Just HourlyTag

        "DAILY" ->
            Just DailyTag

        "WEEKLY" ->
            Just WeeklyTag

        "MONTHLY" ->
            Just MonthlyTag

        "YEARLY" ->
            Just YearlyTag

        _ ->
            Nothing


buildFrequency : FreqTag -> Int -> Time.Weekday -> Frequency
buildFrequency tag interval weekStart =
    case tag of
        SecondlyTag ->
            Secondly { every = interval }

        MinutelyTag ->
            Minutely { every = interval }

        HourlyTag ->
            Hourly { every = interval }

        DailyTag ->
            Daily { every = interval }

        WeeklyTag ->
            Weekly { every = interval, weekStart = weekStart }

        MonthlyTag ->
            Monthly { every = interval }

        YearlyTag ->
            Yearly { every = interval }


parseRecurrenceEnd : Maybe String -> Maybe String -> Result String RecurrenceEnd
parseRecurrenceEnd maybeCount maybeUntil =
    case ( maybeCount, maybeUntil ) of
        ( Just countStr, Nothing ) ->
            case String.toInt countStr of
                Just n ->
                    if n > 0 then
                        Ok (Count n)

                    else
                        Err ("Invalid COUNT: " ++ countStr)

                Nothing ->
                    Err ("Invalid COUNT: " ++ countStr)

        ( Nothing, Just untilStr ) ->
            parseUntilValue untilStr

        ( Nothing, Nothing ) ->
            Ok Forever

        ( Just _, Just _ ) ->
            Err "RRULE cannot have both COUNT and UNTIL"


parseInterval : Maybe String -> Result String Int
parseInterval maybeValue =
    case maybeValue of
        Nothing ->
            Ok 1

        Just intervalStr ->
            case String.toInt intervalStr of
                Just interval ->
                    if interval > 0 then
                        Ok interval

                    else
                        Err ("INTERVAL must be at least 1, got " ++ String.fromInt interval)

                Nothing ->
                    Err ("Invalid INTERVAL: " ++ intervalStr)


parseWeekStart : Maybe String -> Result String Time.Weekday
parseWeekStart maybeValue =
    case maybeValue of
        Nothing ->
            Ok Time.Mon

        Just weekStartStr ->
            case parseWeekday weekStartStr of
                Just weekday ->
                    Ok weekday

                Nothing ->
                    Err ("Invalid WKST: " ++ weekStartStr)


parseIntList : String -> (Int -> Bool) -> Maybe String -> Result String (List Int)
parseIntList fieldName isValid maybeStr =
    case maybeStr of
        Nothing ->
            Ok []

        Just str ->
            str
                |> String.split ","
                |> List.foldr
                    (\part acc ->
                        case ( String.toInt part, acc ) of
                            ( Just value, Ok values ) ->
                                if isValid value then
                                    Ok (value :: values)

                                else
                                    Err ("Invalid " ++ fieldName ++ ": " ++ part)

                            ( Nothing, _ ) ->
                                Err ("Invalid " ++ fieldName ++ ": " ++ part)

                            ( _, Err err ) ->
                                Err err
                    )
                    (Ok [])


isValidMonthDay : Int -> Bool
isValidMonthDay value =
    value /= 0 && value >= -31 && value <= 31


isValidMonth : Int -> Bool
isValidMonth value =
    value >= 1 && value <= 12


isValidSetPos : Int -> Bool
isValidSetPos value =
    value /= 0 && value >= -366 && value <= 366


isValidYearDay : Int -> Bool
isValidYearDay value =
    value /= 0 && value >= -366 && value <= 366


isValidWeekNo : Int -> Bool
isValidWeekNo value =
    value /= 0 && value >= -53 && value <= 53


isValidHour : Int -> Bool
isValidHour value =
    value >= 0 && value <= 23


isValidMinute : Int -> Bool
isValidMinute value =
    value >= 0 && value <= 59


isValidSecond : Int -> Bool
isValidSecond value =
    value >= 0 && value <= 60


type alias ByParts =
    { byMonthDay : List Int
    , byMonth : List Time.Month
    , bySetPos : List Int
    , byHour : List Int
    , byMinute : List Int
    , bySecond : List Int
    , byYearDay : List Int
    , byWeekNo : List Int
    }


parseByParts : (String -> Maybe String) -> Result String ByParts
parseByParts getParam =
    parseIntList "BYMONTHDAY" isValidMonthDay (getParam "BYMONTHDAY")
        |> Result.andThen
            (\byMonthDay ->
                parseIntList "BYMONTH" isValidMonth (getParam "BYMONTH")
                    |> Result.andThen
                        (\byMonthInts ->
                            parseIntList "BYSETPOS" isValidSetPos (getParam "BYSETPOS")
                                |> Result.andThen
                                    (\bySetPos ->
                                        parseIntList "BYHOUR" isValidHour (getParam "BYHOUR")
                                            |> Result.andThen
                                                (\byHour ->
                                                    parseIntList "BYMINUTE" isValidMinute (getParam "BYMINUTE")
                                                        |> Result.andThen
                                                            (\byMinute ->
                                                                parseIntList "BYSECOND" isValidSecond (getParam "BYSECOND")
                                                                    |> Result.andThen
                                                                        (\bySecond ->
                                                                            parseIntList "BYYEARDAY" isValidYearDay (getParam "BYYEARDAY")
                                                                                |> Result.andThen
                                                                                    (\byYearDay ->
                                                                                        parseIntList "BYWEEKNO" isValidWeekNo (getParam "BYWEEKNO")
                                                                                            |> Result.map
                                                                                                (\byWeekNo ->
                                                                                                    { byMonthDay = byMonthDay
                                                                                                    , byMonth = List.filterMap intToMonth byMonthInts
                                                                                                    , bySetPos = bySetPos
                                                                                                    , byHour = byHour
                                                                                                    , byMinute = byMinute
                                                                                                    , bySecond = bySecond
                                                                                                    , byYearDay = byYearDay
                                                                                                    , byWeekNo = byWeekNo
                                                                                                    }
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


intToMonth : Int -> Maybe Time.Month
intToMonth n =
    if n >= 1 && n <= 12 then
        Just (Date.numberToMonth n)

    else
        Nothing


parseUntilValue : String -> Result String RecurrenceEnd
parseUntilValue str =
    if String.length str == 8 then
        -- DATE format: YYYYMMDD
        parseDate str
            |> Result.map
                (\{ year, month, day } ->
                    UntilDate (Date.fromCalendarDate year (Date.numberToMonth month) day)
                )

    else
        -- DATE-TIME format
        parseDateTime str
            |> Result.map
                (\parts ->
                    let
                        date : Date.Date
                        date =
                            Date.fromCalendarDate parts.year (Date.numberToMonth parts.month) parts.day

                        daysSinceEpoch : Int
                        daysSinceEpoch =
                            Date.toRataDie date - 719163

                        totalSeconds : Int
                        totalSeconds =
                            daysSinceEpoch * 86400 + parts.hour * 3600 + parts.minute * 60 + parts.second
                    in
                    UntilDateTime (Time.millisToPosix (totalSeconds * 1000))
                )


parseDaySpecs : Maybe String -> Result String (List DaySpec)
parseDaySpecs maybeStr =
    case maybeStr of
        Nothing ->
            Ok []

        Just str ->
            String.split "," str
                |> List.foldr
                    (\part acc ->
                        case acc of
                            Err err ->
                                Err err

                            Ok specs ->
                                case parseDaySpec part of
                                    Ok spec ->
                                        Ok (spec :: specs)

                                    Err err ->
                                        Err err
                    )
                    (Ok [])


parseDaySpec : String -> Result String DaySpec
parseDaySpec str =
    let
        trimmed : String
        trimmed =
            String.trim str
    in
    -- Try to extract ordinal prefix (e.g., "2SU", "-1FR", "MO")
    case parseWeekday (String.right 2 trimmed) of
        Just weekday ->
            let
                prefix : String
                prefix =
                    String.dropRight 2 trimmed
            in
            if String.isEmpty prefix then
                Ok (Every weekday)

            else
                case String.toInt prefix of
                    Just n ->
                        ordinalToDaySpec n weekday
                            |> Result.fromMaybe ("Invalid BYDAY ordinal: " ++ str)

                    Nothing ->
                        Err ("Invalid BYDAY: " ++ str)

        Nothing ->
            Err ("Invalid BYDAY: " ++ str)


ordinalToDaySpec : Int -> Time.Weekday -> Maybe DaySpec
ordinalToDaySpec n weekday =
    if n > 0 then
        case n of
            1 ->
                Just (Every1st weekday)

            2 ->
                Just (Every2nd weekday)

            3 ->
                Just (Every3rd weekday)

            4 ->
                Just (Every4th weekday)

            5 ->
                Just (Every5th weekday)

            _ ->
                Nothing

    else
        case negate n of
            1 ->
                Just (EveryLast weekday)

            2 ->
                Just (Every2ndToLast weekday)

            3 ->
                Just (Every3rdToLast weekday)

            4 ->
                Just (Every4thToLast weekday)

            5 ->
                Just (Every5thToLast weekday)

            _ ->
                Nothing


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
