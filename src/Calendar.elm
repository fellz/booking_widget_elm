module Calendar exposing
    ( CalendarDay
    , firstOfMonth
    , isCurrentMonth
    , monthGrid
    , shiftMonth
    )

{-| Month-grid generator and navigation. Renders 5 or 6 week rows depending on
how the month falls — a trailing row of only next-month days is dropped. Days
are laid out Monday-first.
-}

import Date exposing (Date)
import Domain.Date exposing (addDays, isSameDay)


type alias CalendarDay =
    { date : Date
    , inCurrentMonth : Bool
    , isToday : Bool
    }


{-| The first calendar day of `date`'s month.
-}
firstOfMonth : Date -> Date
firstOfMonth date =
    Date.fromCalendarDate (Date.year date) (Date.month date) 1


{-| Move `cursor` (a first-of-month date) by `delta` whole months.
-}
shiftMonth : Int -> Date -> Date
shiftMonth delta cursor =
    Date.add Date.Months delta cursor


isCurrentMonth : Date -> Date -> Bool
isCurrentMonth today cursor =
    Date.year cursor == Date.year today && Date.month cursor == Date.month today


{-| The week×7 grid for the month at `cursor` (which must be a first-of-month
date), with `today` flagged. Leading/trailing days from adjacent months are
included and marked `inCurrentMonth = False`. Renders 5 or 6 week rows depending
on how the month falls — a trailing row of only next-month days is dropped.
-}
monthGrid : Date -> Date -> List (List CalendarDay)
monthGrid today cursor =
    let
        -- Date.weekdayNumber is Monday=1 … Sunday=7.
        leading =
            Date.weekdayNumber cursor - 1

        daysInMonth =
            Date.diff Date.Days cursor (Date.add Date.Months 1 cursor)

        weekCount =
            (leading + daysInMonth + 6) // 7

        gridStart =
            addDays (negate leading) cursor

        toDay offset =
            let
                date =
                    addDays offset gridStart
            in
            { date = date
            , inCurrentMonth = Date.month date == Date.month cursor
            , isToday = isSameDay date today
            }
    in
    List.range 0 (weekCount - 1)
        |> List.map (\week -> List.map (\day -> toDay (week * 7 + day)) (List.range 0 6))
