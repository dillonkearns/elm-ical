module TeamCalendar exposing (run)

import Date
import Ical
import Ical.Recurrence as Recurrence
import Pages.Script as Script exposing (Script)
import Time


run : Script
run =
    Script.withoutCliOptions
        (Script.log generatedCalendar)


generatedCalendar : String
generatedCalendar =
    let
        now : Time.Posix
        now =
            Time.millisToPosix 1710748800000

        weeklySync : Ical.Event
        weeklySync =
            Ical.event
                { id = "weekly-sync"
                , stamp = now
                , time =
                    Ical.withTime
                        { start = Time.millisToPosix 1710781200000
                        , end = Time.millisToPosix 1710784800000
                        }
                , summary = "Weekly Team Sync"
                }
                |> Ical.withLocation "Zoom"
                |> Ical.withRecurrenceRule
                    (Ical.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                        |> Ical.withByDay [ Recurrence.every Time.Mon, Recurrence.every Time.Wed ]
                        |> Ical.withCount 6
                    )

        releaseDay : Ical.Event
        releaseDay =
            Ical.event
                { id = "release-day"
                , stamp = now
                , time = Ical.allDay (Date.fromCalendarDate 2024 Time.Mar 12)
                , summary = "Release Day"
                }
                |> Ical.withDescription "Ship the monthly release checklist."
                |> Ical.withRecurrenceRule
                    (Ical.rule (Recurrence.Monthly { every = 1 })
                        |> Ical.withByDay [ Recurrence.second Time.Tue ]
                        |> Ical.withCount 3
                    )
    in
    Ical.generate
        (Ical.config
            { id = "//mycompany//team//EN"
            , domain = "mycompany.com"
            }
            |> Ical.withName "Engineering Team"
        )
        [ weeklySync, releaseDay ]
