module Excurs where


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
leakPerOp :: Float -> Float -> Float -> Float
leakPerOp leak hours operations = leak * 10^6 / (operations * hours * 3600)


data LeakStat = LeakStat
    { perHour :: Float
    , perDay :: Float
    , perWeek :: Float
    }

-- |
-- memory leak per day / week
-- leak in MB
-- hours duration over which leak was observed
-- operations per second - throughput during test
-- returns leak in MB per (hour, day, week)
leakOverTime :: Float -> Float -> LeakStat
leakOverTime leak hours = LeakStat hour day week
    where
        hour = leak / hours
        day  = hour * 24
        week = day * 7

reportOnLeak :: Float -> Float -> Float -> IO ()
reportOnLeak leak hours operations = do
    putStrLn "--- Memory Leak ---"
    putStrLn  $ "Leak of " ++ (show leak) ++ "MB/"
                ++ (show hours) ++ "h at " ++ (show operations) ++ "ops/s"
    putStrLn $ "Operations per day: " ++ (show (operations * 3600 * 24))
    putStr "Leak per op:   "
    putStrLn $ show (leakPerOp leak hours operations) ++ "B"
    putStr "Leak per hour:  "
    putStrLn $ show (perHour leakStats) ++ "MB"
    putStr "Leak per day:  "
    putStrLn $ show (perDay leakStats) ++ "MB"
    putStr "Leak per week: "
    putStrLn $ show (perWeek leakStats) ++ "MB"
    where
        leakStats = leakOverTime leak hours


-- | Logistic map.
-- x(t+1) = Rx(t)(1-x(t))
-- r : parameter R
-- xt: value at time t (initial value)
-- n : number of recursions
-- Example: logMap 3.0 0.7 50
logMap :: Double -> Double -> Int -> [Double]
logMap r xt 0 = [xt]
logMap r xt n = xt : logMap r (r * xt * (1 - xt)) (n - 1)

