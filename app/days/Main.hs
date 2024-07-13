-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

-- | Configure the last day of the relevant interval.
lastDay :: Cal.Day
lastDay = Cal.YearMonthDay  2024 10 31

-- | Configure the list of planned vacation days.
plannedVacationDays :: [Cal.Day]
plannedVacationDays = [toDate 2024 05 27, toDate 2024 05 28, toDate 2024 05 29, toDate 2024 05 31,
                       toDate 2024 07 01, toDate 2024 07 02, toDate 2024 07 03, toDate 2024 07 04, toDate 2024 07 05,
                       toDate 2024 07 08, toDate 2024 07 09, toDate 2024 07 10, toDate 2024 07 11, toDate 2024 07 12]


-- | Calculate remaining days and print a report.
main :: IO ()
main =  do
    firstDay <- today
    let
        daysOfYear = Cal.periodAllDays (Cal.dayPeriod lastDay :: Cal.Year) :: [Cal.Day]
        workDays = filterWorkdays firstDay daysOfYear
        numRemainingCalendardays = Cal.diffDays lastDay firstDay
        numRemainingWorkdays = length workDays
        numRemainingVacationDays = numVacationDays firstDay

    printf "Days from %s to %s -> %d (work days: %d) (after vaccation days (%d): %d)\n"
        (show firstDay) (show lastDay) numRemainingCalendardays numRemainingWorkdays
        numRemainingVacationDays (numRemainingWorkdays - numRemainingVacationDays)


-- | Provide the current day.
today :: IO Cal.Day
today = fmap Cl.utctDay Cl.getCurrentTime

-- | Provide a list of holidays.
holidaysList :: [Cal.Day]
holidaysList = [toDate 2024 5 1, toDate 2024 5 9, toDate 2024 5 20, toDate 2024 5 30, toDate 2024 10 3,
                toDate 2024 12 24, toDate 2024 12 25, toDate 2024 12 26, toDate 2024 12 31]

-- | Number of open vacation days.
numVacationDays :: Cal.Day -> Int
numVacationDays first = unplannedVacationDays + length (futureVacationDays first)

-- | List of future and present vacation days.
-- futureVacationDays :: Cal.Day -> Cal.Day -> [Cal.Day]
-- futureVacationDays first last = (filter (first <=) . filter (last >=)) plannedVacationDays
futureVacationDays :: Cal.Day -> [Cal.Day]
futureVacationDays first = (filter (first <=) . filter (lastDay >=)) plannedVacationDays

-- | Number of unplanned vacation days
unplannedVacationDays :: Int
unplannedVacationDays = 25 - length plannedVacationDays  -- 4d: 27.5.-31.5

-- |  Filter for workdays in the given interval.
filterWorkdays :: Cal.Day -> [Cal.Day] -> [Cal.Day]
filterWorkdays first = filter (`notElem` holidaysList) .
        filter (\d -> Cal.dayOfWeek d `notElem` [Cal.Saturday, Cal.Sunday]) .
        filter (lastDay >=) .
        filter (first <=)

-- | Create a date
toDate :: Cal.Year -> Cal.MonthOfYear -> Cal.DayOfMonth  -> Cal.Day
toDate = Cal.fromGregorian

