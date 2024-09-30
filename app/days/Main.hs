-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

-- | Configure date of retirement.
dayBeforeRetirement :: Cal.Day
dayBeforeRetirement = Cal.YearMonthDay  2024 10 31

-- | Configure the last day of the relevant contract.
lastDay :: Cal.Day
lastDay = dayBeforeRetirement
--lastDay = Cal.YearMonthDay 2025 10 31

-- | Provide the number of granted vacation days since and including 2024
numberOfGrantedVacationDays :: Int
numberOfGrantedVacationDays = 25

-- | Provide the list of planned vacation days.
plannedVacationDays :: [Cal.Day]
plannedVacationDays = [toDate 2024 05 27, toDate 2024 05 28, toDate 2024 05 29, toDate 2024 05 31,
                       toDate 2024 07 01, toDate 2024 07 02, toDate 2024 07 03, toDate 2024 07 04, toDate 2024 07 05,
                       toDate 2024 07 08, toDate 2024 07 09, toDate 2024 07 10, toDate 2024 07 11, toDate 2024 07 12,
                       toDate 2024 09 09, toDate 2024 09 10, toDate 2024 09 11, toDate 2024 09 12, toDate 2024 09 13,
                       toDate 2024 09 30, toDate 2024 10 01, toDate 2024 10 02]

-- | Provide the list of holidays.
holidaysList :: [Cal.Day]
holidaysList = [toDate 2024 5 1, toDate 2024 5 9, toDate 2024 5 20, toDate 2024 5 30, toDate 2024 10 3,
                toDate 2024 12 24, toDate 2024 12 25, toDate 2024 12 26, toDate 2024 12 31,
                toDate 2025 01 01, toDate 2025 03 03, toDate 2025 04 18, toDate 2025 04 21, toDate 2025 05 01,
                toDate 2025 05 29, toDate 2025 06 09, toDate 2025 06 19, toDate 2025 10 03, toDate 2025 11 01,
                toDate 2025 12 24, toDate 2025 12 25, toDate 2025 12 26, toDate 2025 12 31]


-- | Calculate remaining days and print a report.
main :: IO ()
main =  do
    firstDay <- today
    let
        daysOfYear = days (Cal.dayPeriod firstDay) (Cal.dayPeriod dayBeforeRetirement)
        listOfWorkDays = filterWorkdays firstDay daysOfYear
        numRemainingCalendardays = 1 + Cal.diffDays dayBeforeRetirement firstDay
        numRemainingWorkdays = length listOfWorkDays
        numRemainingVacationDays = numVacationDays firstDay

    printf "Days from %s to %s -> %d (work days: %d) (after vaccation days (%d): %d)\n"
        (show firstDay) (show dayBeforeRetirement) numRemainingCalendardays numRemainingWorkdays
        numRemainingVacationDays (numRemainingWorkdays - numRemainingVacationDays)
    -- printf "DaysOfYears: %s\n" (show daysOfYear)


-- | Generate a list of days for the given years and years in between.
days :: Integer -> Integer -> [Cal.Day]
days firstYear lastYear
    | firstYear == lastYear = Cal.periodAllDays (Cal.dayPeriod (Cal.YearMonthDay firstYear 01 01) :: Cal.Year) :: [Cal.Day]
    | firstYear < lastYear = Cal.periodAllDays (Cal.dayPeriod (Cal.YearMonthDay firstYear 01 01) :: Cal.Year) ++ days (firstYear + 1) lastYear
    | otherwise = error "first year must be less or equal to last year"

-- | Provide the current day.
today :: IO Cal.Day
today = fmap Cl.utctDay Cl.getCurrentTime

-- | Number of open vacation days.
numVacationDays :: Cal.Day -> Int
numVacationDays currentDay = unplannedVacationDays + length (futureVacationDays currentDay)

-- | Number of unplanned vacation days
unplannedVacationDays :: Int
unplannedVacationDays = numberOfGrantedVacationDays - length plannedVacationDays

-- | List of future and present vacation days.
futureVacationDays :: Cal.Day -> [Cal.Day]
futureVacationDays currentDay = (filter (currentDay <=) . filter (dayBeforeRetirement >=)) plannedVacationDays

-- |  Filter for workdays in the given interval.
filterWorkdays :: Cal.Day -> [Cal.Day] -> [Cal.Day]
filterWorkdays currentDay = filter (`notElem` holidaysList) .
        filter (\d -> Cal.dayOfWeek d `elem` workDays) .
        filter (dayBeforeRetirement >=) .
        filter (currentDay <=)
        where
            workDays :: [Cal.DayOfWeek]
            workDays | currentDay <= dayBeforeRetirement = [Cal.Monday, Cal.Tuesday, Cal.Wednesday, Cal.Thursday, Cal.Friday]
                     | otherwise = [Cal.Monday, Cal.Tuesday, Cal.Wednesday]

-- | Create a date
toDate :: Cal.Year -> Cal.MonthOfYear -> Cal.DayOfMonth  -> Cal.Day
toDate = Cal.fromGregorian

