-- | Count days to a given date.
--
module Main where

import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Cl
import Text.Printf

main :: IO ()
main =  do
    now <- Cl.getCurrentTime
    let end = Cal.fromGregorian 2024 10 31
        today = Cl.utctDay now
        diffDays = Cal.diffDays end today

    printf "Days from %s to %s -> %d (work days %d)\n" (show today) (show end) diffDays (workDays diffDays)

-- | Approximate number of weekend days for a given span of days.
workDays :: Integer -> Integer
workDays days = days * 5 `div` 7

