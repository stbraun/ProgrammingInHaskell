-- | Calculates memory leak for a timespan.
-- Call evalLeak with an array of memory usages, an array of the (relative) time in hours for each memory data point, and the number of requests per second. E.g.:
-- `evalLeak [200.5, 25.7] [0 1] 34`
--
module Leak where

import qualified Data.Text as Text
import Text.Printf

-- | Calculate and report memory usage for a set of timespans.
-- privBytes: array of memory usage data points in MB. E.g., [201.3, 202.4, 203.8].
-- hours: array of time offsets between data points. E.g., [0, 1, 2.5].
-- opsPerSec: number of operations per second averaged over the test run. E.g., 34.7.
-- Prints a report with leakage per request/hour/day/week/month/year.
evalLeak :: [Double] -> [Double] -> Double -> IO ()
evalLeak privbytes hours opsPerSec = reportOnLeak leak duration opsPerSec
    where
        leak = (head . reverse) privbytes - (head privbytes)
        duration = (head . reverse) hours - (head hours)

-- |
-- memory leak per operation
-- leak in MB
-- hours duration over which leak was observed
-- operations per second - throughput during test
-- returns leak in Byte/operation
leakPerOp :: Double -> Double -> Double -> Double
leakPerOp leak hours operations = leak * 10^6 / (operations * hours * 3600)


data LeakStat = LeakStat
    { perHour :: Double
    , perDay :: Double
    , perWeek :: Double
    , perMonth :: Double -- 30days/month
    , perYear :: Double -- 12months
    }

-- |
-- memory leak per day / week
-- leak in MB
-- hours duration over which leak was observed
-- operations per second - throughput during test
-- returns leak in MB per (hour, day, week)
leakOverTime :: Double -> Double -> LeakStat
leakOverTime leak hours = LeakStat hour day week month year
    where
        hour = leak / hours
        day  = hour * 24
        week = day * 7
        month = day * 30
        year = month * 12

reportOnLeak :: Double -> Double -> Double -> IO ()
reportOnLeak leak hours operations = do
    putStrLn "--- Memory Leak ---"
    printf "Leak of %.2fMB over %.1fh at %.1fops/s\n" leak hours operations
    printf "Operations per day: %.0f\n" (operations * 3600 * 24)
    printf "Leak per op:    %7.2f B\n" $ leakPerOp leak hours operations
    printf "Leak per hour:  %7.2f MB\n" (perHour leakStats)
    printf "Leak per day:   %7.2f MB\n" (perDay leakStats)
    printf "Leak per week:  %7.2f MB\n" (perWeek leakStats)
    printf "Leak per month: %7.2f MB\n" (perMonth leakStats)
    printf "Leak per year:  %7.2f MB\n" (perYear leakStats)
    where
        leakStats = leakOverTime leak hours

printRawLeakData :: [Double] -> [Double] -> IO ()
printRawLeakData privbytes hours = do
    printf "Time privBytes [MB]\n"
    printf "-------------------\n"
    printData rawData
    where
        printData d = putStrLn $ (Text.unpack
                                    . Text.intercalate (Text.pack "\n")
                                    . map Text.pack
                                    . map show) d
        rawData = [(hour, mbyte) | (hour, mbyte) <- zip hours privbytes]


