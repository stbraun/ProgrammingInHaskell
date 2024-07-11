-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

main :: IO ()
main =  do
    let
        end = toDate 2024 10 31
    days end

-- | Print workdays from current date to given date.
days :: Cal.Day -> IO ()
days end = do
    now <- Cl.getCurrentTime
    let
        today = Cl.utctDay now
        daysOfYear = Cal.periodAllDays (Cal.dayPeriod (Cal.YearMonthDay 2024 5 2 :: Cal.Day) :: Cal.Year) :: [Cal.Day]

        workDays = filterWorkdays today end daysOfYear

        numRemainingCalendardays = Cal.diffDays end today
        numRemainingWorkdays = length workDays
        numRemainingVacationDays = numVacationDays today end

    printf "Days from %s to %s -> %d (work days: %d) (after vaccation days (%d): %d)\n"
        (show today) (show end) numRemainingCalendardays numRemainingWorkdays
        numRemainingVacationDays (numRemainingWorkdays - numRemainingVacationDays)


-- | Provide a list of holidays.
holidaysList :: [Cal.Day]
holidaysList = [toDate 2024 5 1, toDate 2024 5 9, toDate 2024 5 20, toDate 2024 5 30, toDate 2024 10 3,
                toDate 2024 12 24, toDate 2024 12 25, toDate 2024 12 26, toDate 2024 12 31]

-- | Number of open vacation days.
numVacationDays :: Cal.Day -> Cal.Day -> Int
numVacationDays first last = unplannedVacationDays + length (futureVacationDays first last)

-- | List of planned vacation days
plannedVacationDays :: [Cal.Day]
plannedVacationDays = [toDate 2024 05 27, toDate 2024 05 28, toDate 2024 05 29, toDate 2024 05 31,
                       toDate 2024 07 01, toDate 2024 07 02, toDate 2024 07 03, toDate 2024 07 04, toDate 2024 07 05,
                       toDate 2024 07 08, toDate 2024 07 09, toDate 2024 07 10, toDate 2024 07 11, toDate 2024 07 12]

-- | List of future and present vacation days.
futureVacationDays :: Cal.Day -> Cal.Day -> [Cal.Day]
futureVacationDays first last = (filter (first <=) . filter (last >=)) plannedVacationDays

-- | Number of unplanned vacation days
unplannedVacationDays :: Int
unplannedVacationDays = 25 - length plannedVacationDays  -- 4d: 27.5.-31.5

-- | Filter for workdays in the given interval.
filterWorkdays :: Cal.Day -> Cal.Day -> [Cal.Day] -> [Cal.Day]
filterWorkdays first last = filter (`notElem` holidaysList) .
        filter (\d -> Cal.Saturday /= Cal.dayOfWeek d && Cal.Sunday /= Cal.dayOfWeek d) .
        filter (last >=) .
        filter (first <=)

-- | Create a date
toDate :: Cal.Year -> Cal.MonthOfYear -> Cal.DayOfMonth  -> Cal.Day
toDate = Cal.fromGregorian

