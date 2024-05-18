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
        holidaysList = [toDate 2024 5 1, toDate 2024 5 9, toDate 2024 5 20, toDate 2024 5 30, toDate 2024 10 3,
                        toDate 2024 12 24, toDate 2024 12 25, toDate 2024 12 26, toDate 2024 12 31]
        holidays = toInteger $ (length . filter (end >) . filter (today <)) holidaysList
        diffDays = Cal.diffDays end today
        remWorkDays = workDays diffDays
        remVacancies = remWorkDays - vacationDays
        remHolidays = remVacancies - holidays

    printf "Days from %s to %s -> %d (work days: %d) (after vacancies and holidays: %d)\n" (show today) (show end) diffDays remWorkDays remHolidays


-- | Create a date
toDate :: Cal.Year -> Cal.MonthOfYear -> Cal.DayOfMonth  -> Cal.Day
toDate = Cal.fromGregorian


-- | Approximate number of workdays for a given span of days.
workDays :: Integer -> Integer
workDays days = days * 5 `div` 7

-- | Approximate number of vacation days
vacationDays :: Integer
vacationDays = 10 * 30 `div` 12  -- 4d: 27.5.-31.5

