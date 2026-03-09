module ValueParser exposing (DateTimeParts, Duration, parseDate, parseDateTime, parseDuration, parseRecurrenceRule, unescapeText)

{-| Parsers for iCal value types: DATE, DATE-TIME, DURATION, RECUR, and TEXT unescaping.
-}

import Date
import Ical.Recurrence exposing (DaySpec, Frequency(..), RecurrenceEnd(..), RecurrenceRule)
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
                Ok { year = year, month = month, day = day }

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
                            Ok
                                { year = year
                                , month = month
                                , day = day
                                , hour = hour
                                , minute = minute
                                , second = second
                                , isUtc = hasZ
                                }

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
    let
        stripped : String
        stripped =
            if String.startsWith "+" input then
                String.dropLeft 1 input

            else if String.startsWith "-" input then
                -- Negative durations exist in spec but we treat as positive
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
            case String.toInt (String.replace "W" "" afterP) of
                Just w ->
                    Ok { weeks = w, days = 0, hours = 0, minutes = 0, seconds = 0 }

                Nothing ->
                    Err ("Invalid DURATION: " ++ input)

        else
            case String.split "T" afterP of
                [ datePart, timePart ] ->
                    let
                        days : Int
                        days =
                            parseDurationDays datePart
                    in
                    case parseDurationTime timePart of
                        Ok time ->
                            Ok { weeks = 0, days = days, hours = time.hours, minutes = time.minutes, seconds = time.seconds }

                        Err err ->
                            Err err

                [ datePart ] ->
                    if String.isEmpty datePart then
                        Err ("Invalid DURATION: " ++ input)

                    else
                        Ok { weeks = 0, days = parseDurationDays datePart, hours = 0, minutes = 0, seconds = 0 }

                _ ->
                    Err ("Invalid DURATION: " ++ input)


parseDurationDays : String -> Int
parseDurationDays part =
    if String.isEmpty part then
        0

    else
        part
            |> String.replace "D" ""
            |> String.toInt
            |> Maybe.withDefault 0


parseDurationTime : String -> Result String { hours : Int, minutes : Int, seconds : Int }
parseDurationTime part =
    if String.isEmpty part then
        Err "Empty time part in DURATION"

    else
        Ok (parseDurationTimeHelp part { hours = 0, minutes = 0, seconds = 0 })


parseDurationTimeHelp : String -> { hours : Int, minutes : Int, seconds : Int } -> { hours : Int, minutes : Int, seconds : Int }
parseDurationTimeHelp remaining acc =
    if String.isEmpty remaining then
        acc

    else
        let
            ( digits, rest ) =
                spanDigits remaining
        in
        case ( String.toInt digits, String.left 1 rest ) of
            ( Just n, "H" ) ->
                parseDurationTimeHelp (String.dropLeft 1 rest) { acc | hours = n }

            ( Just n, "M" ) ->
                parseDurationTimeHelp (String.dropLeft 1 rest) { acc | minutes = n }

            ( Just n, "S" ) ->
                parseDurationTimeHelp (String.dropLeft 1 rest) { acc | seconds = n }

            _ ->
                acc


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

                Just frequency ->
                    let
                        interval : Int
                        interval =
                            getParam "INTERVAL"
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 1

                        weekStart : Time.Weekday
                        weekStart =
                            getParam "WKST"
                                |> Maybe.andThen parseWeekday
                                |> Maybe.withDefault Time.Mon
                    in
                    parseRecurrenceEnd (getParam "COUNT") (getParam "UNTIL")
                        |> Result.andThen
                            (\end ->
                                parseDaySpecs (getParam "BYDAY")
                                    |> Result.map
                                        (\byDay ->
                                            { frequency = frequency
                                            , interval = interval
                                            , end = end
                                            , byDay = byDay
                                            , byMonthDay = parseIntList (getParam "BYMONTHDAY")
                                            , byMonth = parseIntList (getParam "BYMONTH")
                                            , bySetPos = parseIntList (getParam "BYSETPOS")
                                            , weekStart = weekStart
                                            }
                                        )
                            )


parseFrequency : String -> Maybe Frequency
parseFrequency str =
    case String.toUpper str of
        "DAILY" ->
            Just Daily

        "WEEKLY" ->
            Just Weekly

        "MONTHLY" ->
            Just Monthly

        "YEARLY" ->
            Just Yearly

        _ ->
            Nothing


parseRecurrenceEnd : Maybe String -> Maybe String -> Result String RecurrenceEnd
parseRecurrenceEnd maybeCount maybeUntil =
    case ( maybeCount, maybeUntil ) of
        ( Just countStr, Nothing ) ->
            case String.toInt countStr of
                Just n ->
                    Ok (Count n)

                Nothing ->
                    Err ("Invalid COUNT: " ++ countStr)

        ( Nothing, Just untilStr ) ->
            parseUntilValue untilStr

        ( Nothing, Nothing ) ->
            Ok Forever

        ( Just _, Just _ ) ->
            Err "RRULE cannot have both COUNT and UNTIL"


parseUntilValue : String -> Result String RecurrenceEnd
parseUntilValue str =
    if String.length str == 8 then
        -- DATE format: YYYYMMDD
        parseDate str
            |> Result.map
                (\{ year, month, day } ->
                    UntilDate (Date.fromCalendarDate year (intToMonth month) day)
                )

    else
        -- DATE-TIME format
        parseDateTime str
            |> Result.map
                (\parts ->
                    let
                        date : Date.Date
                        date =
                            Date.fromCalendarDate parts.year (intToMonth parts.month) parts.day

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
                Ok { ordinal = Nothing, weekday = weekday }

            else
                case String.toInt prefix of
                    Just n ->
                        Ok { ordinal = Just n, weekday = weekday }

                    Nothing ->
                        Err ("Invalid BYDAY: " ++ str)

        Nothing ->
            Err ("Invalid BYDAY: " ++ str)


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


parseIntList : Maybe String -> List Int
parseIntList maybeStr =
    case maybeStr of
        Nothing ->
            []

        Just str ->
            String.split "," str
                |> List.filterMap String.toInt


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
