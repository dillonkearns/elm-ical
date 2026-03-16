module TeamCalendar exposing (run)

import Date
import Ical.Generator as Generator
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

        weeklySync : Generator.Event
        weeklySync =
            Generator.event
                { id = "weekly-sync"
                , stamp = now
                , time =
                    Generator.timedEvent
                        { start = Time.millisToPosix 1710781200000
                        , end = Time.millisToPosix 1710784800000
                        }
                , summary = "Weekly Team Sync"
                }
                |> Generator.withLocation "Zoom"
                |> Generator.withRecurrenceRule
                    (Generator.rule (Recurrence.Weekly { every = 1, weekStart = Time.Mon })
                        |> Generator.withByDay [ Recurrence.every Time.Mon, Recurrence.every Time.Wed ]
                        |> Generator.withCount 6
                    )

        releaseDay : Generator.Event
        releaseDay =
            Generator.event
                { id = "release-day"
                , stamp = now
                , time = Generator.allDay (Date.fromCalendarDate 2024 Time.Mar 12)
                , summary = "Release Day"
                }
                |> Generator.withDescription "Ship the monthly release checklist."
                |> Generator.withRecurrenceRule
                    (Generator.rule (Recurrence.Monthly { every = 1 })
                        |> Generator.withByDay [ Recurrence.second Time.Tue ]
                        |> Generator.withCount 3
                    )
    in
    Generator.generate
        (Generator.config
            { id = "//mycompany//team//EN"
            , domain = "mycompany.com"
            }
            |> Generator.withName "Engineering Team"
        )
        { events = [ weeklySync, releaseDay ]
        , journals = []
        }
