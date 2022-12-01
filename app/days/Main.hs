-- | Count days to retirement.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

main :: IO ()
main =  do
    let end = Cal.fromGregorian 2024 10 31
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
vacationDays = y2024 + y2023 + y2022
    where
        y2022 = 16
        y2023 = 30
        y2024 = 10 * 30 `div` 12

-- | Approximate number of holidays
holidays :: Integer
holidays = y2022 + y2023 + y2024
    where
        y2022 = 1
        y2023 = 10
        y2024 = 5
 
