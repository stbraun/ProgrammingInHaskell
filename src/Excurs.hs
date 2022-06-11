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


-- |
-- memory leak per day / week
-- leak in MB
-- hours duration over which leak was observed
-- operations per second - throughput during test
-- returns leak in MB per (day, week)
leakOverTime :: Float -> Float -> (Float, Float)
leakOverTime leak hours = (day, week)
    where
        day = leak * 24 / hours
        week = day * 7

reportOnLeak :: Float -> Float -> Float -> IO ()
reportOnLeak leak hours operations = do
    putStrLn "--- Memory Leak ---"
    putStrLn  $ "Leak of " ++ (show leak) ++ "MB/"
                ++ (show hours) ++ "h at " ++ (show operations) ++ "ops/s"
    putStrLn $ "Operations per day: " ++ (show (operations * 3600 * 24))
    putStr "Leak per op:   "
    putStrLn $ show (leakPerOp leak hours operations) ++ "B"
    putStr "Leak per day:  "
    putStrLn $ show (fst (leakOverTime leak hours)) ++ "MB"
    putStr "Leak per week: "
    putStrLn $ show (snd (leakOverTime leak hours)) ++ "MB"

