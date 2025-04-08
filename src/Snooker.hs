module Snooker where

-- | Some calculations related to Snooker.

-- | The maximum points available with a given number of red balls.
points :: Int -> Either String Int
points reds | reds < 0 = Left "Number of red balls cannot be negative."
            | reds > 15 = Left "Number of red balls cannot exceed 15."
            | otherwise = Right (reds * 8 + 27)


