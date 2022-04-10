module Graph where

-- A simple path search on a directed graph.

newtype DAG a = DAG [(a,a)]
    deriving (Show)

getDAG :: DAG a -> [(a,a)]
getDAG (DAG x) = x


ex1 :: DAG Int
ex1 = DAG [(1,2), (3,5), (2,6), (6,3)]


-- Verify existence of path between two nodes.
hasPath :: DAG Int -> Int -> Int -> Bool
hasPath (DAG []) _ _ = False
hasPath (DAG (n:ns)) start end
            | fst n == start && snd n /= end = hasPath (DAG ns) (snd n) end
            | fst n == start && snd n == end = True
            | otherwise = let next = filter (\(a,b) -> a == start) ns
                          in
                            if null next
                                then False
                                else let node = next !! 0
                                         tail = filter (\x -> x /= node) (n:ns)
                                     in
                                         hasPath (DAG (node:tail)) start end



