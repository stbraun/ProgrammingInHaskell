module Tree where

import Data.Monoid
import Data.Foldable


-- Simple binary tree with values on nodes.
data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving Show


instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x) = x
    fold (Node x l r) = x `mappend` (fold l) `mappend` (fold r)

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x) = f x
    foldMap f (Node x l r) = f x `mappend` (foldMap f l) `mappend` (foldMap f r)

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f acc (Leaf x) = f x acc
    foldr f acc (Node x l r) = f x (foldr f (foldr f acc r) l)

    -- foldl :: (b -> a -> b) -> b -> Tree a -> b
    foldl f acc (Leaf x) = f acc x
    foldl f acc (Node x l r) = f (foldl f (foldl f acc l) r) x


-- Examples of trees for test-running the folds:
--
-- Type Int of t1 is not a Monoid, so, only foldr and foldl will work on it.
ex1 :: Tree Int
ex1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))

-- Type (Sum Int) of tree t2 is a Monoid with identity=Sum 0 and operation (+).
ex2 :: Tree (Sum Int)
ex2 = Node (Sum 1) (Node (Sum 2) (Leaf (Sum 3)) (Leaf (Sum 4))) (Node (Sum 5) (Leaf (Sum 6)) (Leaf (Sum 7)))

-- Running folds in REPL:
--
-- ex2 cannot run with fold and foldMap because Int is not a Monoid.
-- ghci> foldr (+) 0 ex1
-- 28
-- ghci> foldl (+) 0 ex1
-- 28
-- ex2 works with all four fold functions because (Sum Int) is a Monoid:
-- ghci> fold ex2
-- Sum {getSum = 28}
-- ghci> foldMap (*2) ex2
-- Sum {getSum = 56}
-- ghci> foldr (+) 0 ex2
-- Sum {getSum = 28}
-- ghci> foldl (+) 0 ex2
-- Sum {getSum = 28}

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node x l r) = Node (g x) (fmap g l) (fmap g r)


instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x) = pure Leaf <*> g x
    traverse g (Node x l r) = pure Node <*> g x <*> traverse g l <*> traverse g r

-- example function to test traverse
dec n = if n > 0 then Just (n-1) else Nothing

