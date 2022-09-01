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

    printf "Days from %s to %s -> %d\n" (show today) (show end) (Cal.diffDays end today)

