module UsHolidays exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Http
import Date
import FatalError exposing (FatalError)
import Ical.Parser as Parser
import Pages.Script as Script exposing (Script)
import Time


run : Script
run =
    Script.withoutCliOptions
        (BackendTask.Http.get
            "https://calendar.google.com/calendar/ical/en.usa%23holiday%40group.v.calendar.google.com/public/basic.ics"
            BackendTask.Http.expectString
            |> BackendTask.allowFatal
            |> BackendTask.andThen
                (\icsString ->
                    case Parser.parse icsString of
                        Ok cal ->
                            let
                                upcoming =
                                    cal.events
                                        |> List.filterMap toHoliday
                                        |> List.filter (\h -> Date.compare h.date (Date.fromCalendarDate 2026 Time.Jan 1) /= LT)
                                        |> List.filter (\h -> Date.compare h.date (Date.fromCalendarDate 2027 Time.Jan 1) == LT)
                                        |> List.sortBy (\h -> Date.toRataDie h.date)
                            in
                            Script.log ("US Holidays in 2026 (" ++ String.fromInt (List.length upcoming) ++ " events):\n")
                                |> BackendTask.andThen
                                    (\() ->
                                        upcoming
                                            |> List.map (\h -> Script.log ("  " ++ Date.format "y-MM-dd  EEE  " h.date ++ h.name))
                                            |> BackendTask.sequence
                                            |> BackendTask.map (\_ -> ())
                                    )

                        Err err ->
                            Script.log ("Parse error: " ++ err)
                )
        )


type alias Holiday =
    { name : String
    , date : Date.Date
    }


toHoliday : Parser.Event -> Maybe Holiday
toHoliday event =
    case ( event.summary, event.time ) of
        ( Just name, Parser.AllDay { start } ) ->
            Just { name = name, date = start }

        _ ->
            Nothing
