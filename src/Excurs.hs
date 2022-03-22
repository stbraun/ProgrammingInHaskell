module Excurs where


-- safe add
sadd :: Num a => Maybe a -> Maybe a -> Maybe a
sadd Nothing _ = Nothing
sadd _ Nothing = Nothing
sadd (Just x) (Just y) = Just (x + y)

