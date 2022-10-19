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

collatz :: Int -> IO ()
collatz n = let iterations = collatz' n 0
         in printf "collatz(%d) terminated after %d iterations\n" n iterations

coll :: [Int] -> IO ()
coll ns = putStrLn $ (Text.unpack . Text.intercalate (Text.pack "\n") . map Text.pack . map show)  [(n, collatz' n 0) | n <- ns]

