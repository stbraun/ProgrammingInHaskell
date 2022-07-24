module Excurs where

import qualified Data.Text as Text
import Text.Printf


-- safe div
sdiv :: Maybe Double -> Maybe Double -> Maybe Double
sdiv Nothing _ = Nothing
sdiv _ Nothing = Nothing
sdiv (Just n) (Just m) = if m==0 then Nothing else Just (n / m)


-- safe add
sadd :: Num a => Maybe a -> Maybe a -> Maybe a
sadd Nothing _ = Nothing
sadd _ Nothing = Nothing
sadd (Just x) (Just y) = Just (x + y)


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
    }

-- |
-- memory leak per day / week
-- leak in MB
-- hours duration over which leak was observed
-- operations per second - throughput during test
-- returns leak in MB per (hour, day, week)
leakOverTime :: Double -> Double -> LeakStat
leakOverTime leak hours = LeakStat hour day week
    where
        hour = leak / hours
        day  = hour * 24
        week = day * 7

reportOnLeak :: Double -> Double -> Double -> IO ()
reportOnLeak leak hours operations = do
    putStrLn "--- Memory Leak ---"
    printf "Leak of %.2fMB over %.1fh at %.1fops/s\n" leak hours operations
    printf "Operations per day: %.0f\n" (operations * 3600 * 24)
    printf "Leak per op:   %7.1f B\n" $ leakPerOp leak hours operations
    printf "Leak per hour: %7.2f MB\n" (perHour leakStats)
    printf "Leak per day:  %7.2f MB\n" (perDay leakStats)
    printf "Leak per week: %7.2f MB\n" (perWeek leakStats)
    where
        leakStats = leakOverTime leak hours

evalLeak :: [Double] -> [Double] -> Double -> IO ()
evalLeak privbytes hours opsPerSec = reportOnLeak leak duration opsPerSec
    where
        leak = (head . reverse) privbytes - (head privbytes)
        duration = (head . reverse) hours - (head hours)


durations = [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 36] :: [Double]
privbytes = [200.5, 201.0, 201.4, 202.0, 202.3, 202.6, 202.9, 203.7, 204.0, 204.3, 204.6, 205.0, 207.2, 209.1] :: [Double]


-- | Logistic map.
-- x(t+1) = Rx(t)(1-x(t))
-- r : parameter R
-- xt: value at time t (initial value)
-- n : number of recursions
-- Example: logMap 3.0 0.7 50
logMap :: Double -> Double -> Int -> [Double]
logMap r xt 0 = [xt]
logMap r xt n = xt : logMap r (r * xt * (1 - xt)) (n - 1)


-- Collatz Conjecture
collatz' :: Int -> Int -> Int
collatz' 1 i = i
collatz' n i
    | n < 1 = error "Input must be > 0"
    | even n = collatz' (n `div` 2) (i +1)
    | otherwise = collatz' ( (3 * n + 1) `div` 2) (i + 1)
    where even n = n `mod` 2 == 0

collatz :: Int -> IO ()
collatz n = let iterations = collatz' n 0
         in printf "collatz(%d) terminated after %d iterations\n" n iterations

coll :: [Int] -> IO ()
coll ns = putStrLn $ (Text.unpack . Text.intercalate (Text.pack "\n") . map Text.pack . map show)  [(n, collatz' n 0) | n <- ns]

