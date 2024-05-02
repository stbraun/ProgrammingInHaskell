-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

main :: IO ()
main =  do
    let end = Cal.fromGregorian 2024 10 30
    days end

-- | Print workdays from current date to given date.
days :: Cal.Day -> IO ()
days end = do
    now <- Cl.getCurrentTime
    let today = Cl.utctDay now
        diffDays = Cal.diffDays end today
        remWorkDays = workDays diffDays
        remVacancies = remWorkDays - vacationDays
        remHolidays = remVacancies - holidays

    printf "Days from %s to %s -> %d (work days: %d) (after vacancies and holidays: %d)\n" (show today) (show end) diffDays remWorkDays remHolidays

-- | Approximate number of workdays for a given span of days.
workDays :: Integer -> Integer
workDays days = days * 5 `div` 7

-- | Approximate number of vacation days
vacationDays :: Integer
vacationDays = y2024 + y2023
    where
        y2023 = 30 - 5 - 15 - 5 - 5
        y2024 = 10 * 30 `div` 12

-- | Approximate number of holidays
holidays :: Integer
holidays = y2023 + y2024
    where
        y2023 = 0 --
        y2024 = 3 -- 9.5., 20.5., 3.10.

