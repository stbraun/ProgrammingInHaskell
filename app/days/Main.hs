-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

main :: IO ()
main =  do
    let
        end = toDate 2024 10 30
    days end

-- | Print workdays from current date to given date.
days :: Cal.Day -> IO ()
days end = do
    now <- Cl.getCurrentTime
    let today = Cl.utctDay now
        daysOfYear = Cal.periodAllDays (Cal.dayPeriod (Cal.YearMonthDay 2024 5 2 :: Cal.Day) :: Cal.Year) :: [Cal.Day]

        holidaysList = [toDate 2024 5 1, toDate 2024 5 9, toDate 2024 5 20, toDate 2024 5 30, toDate 2024 10 3,
                        toDate 2024 12 24, toDate 2024 12 25, toDate 2024 12 26, toDate 2024 12 31]
        workDays = (filter (`notElem` holidaysList) .
                    filter (\d -> Cal.Saturday /= Cal.dayOfWeek d) .
                    filter (\d -> Cal.Sunday /= Cal.dayOfWeek d) .
                    filter (end >=) .
                    filter (today <)) daysOfYear

        holidays = (length . filter (end >) . filter (today <)) holidaysList
        diffDays = Cal.diffDays end today
        remWorkDays = length workDays

    printf "Days from %s to %s -> %d (work days: %d) (after vacaction days (%d): %d)\n"
        (show today) (show end) diffDays remWorkDays vacationDays (remWorkDays - vacationDays)


-- | Create a date
toDate :: Cal.Year -> Cal.MonthOfYear -> Cal.DayOfMonth  -> Cal.Day
toDate = Cal.fromGregorian

-- | Approximate number of vacation days
vacationDays :: Int
vacationDays = 25 - 4  -- 4d: 27.5.-31.5

