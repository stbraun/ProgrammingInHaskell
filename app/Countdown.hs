module Countdown where

-- -------------------------------------------------
-- Implementation of the countdown problem as described
-- in Chapter 9 of Programming in Haskell.
-- -------------------------------------------------

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)


-- Supported operations.
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Validate application of an operator.
-- Exploiting algebraic properties to reduce to number of expressions.
-- Commutativity: a + b = b + a, a * b = b * a
-- Identity: x * 1 = 1 * x = x, x / 1 = x
-- Rules:
--   - Values must be positive naturals (1,2,3,...)
--   - Result of operation must also be a positive natural.
--   - Multiplication and division by 1 will be denied.
--   - Duplicates via commutativity must be denied.
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- Apply operator to arguments.
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- An expression can either be an integer or the application of an operator
-- to two argument expressions.
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                        where
                            brak (Val n) = show n
                            brak e       = "(" ++ show e ++ ")"

-- Calculate a list of all values in an expression.
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- Calculate the value of an expression.
-- The result is provided as a singleton list.
-- An empty list is returned if calculation failed, e.g. caused by negative value.
-- Using a list as result value instead of a Maybe allows the use of a list comprehension.
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- Return all subsequences of a list, which are given by all possible combinations
-- of excluding or including each element of the list.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- Return all possible ways of inserting a new element into a list.
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Return all permutations of a list, which are given by all
-- possible reorderings of the elements.
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Returning all choices from a list can be defined by considering
-- all permutations of all subsequences.
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- An expression is a solution for a given list of numbers and a target
-- if the list of values in the expression is chosen from the list of numbers,
-- and the expression successfully evaluates to give the target.
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- ------------------------------------------------------------------
--               ----- Example -----
expr :: Expr
expr = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
numbers = [1,3,7,10,25,50] :: [Int]
target = 765 :: Int
-- ------------------------------------------------------------------

-- Return a list of all pairs of non-empty sublists such that the
-- sublists of each pair append to give the original list.
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]

-- Generate a list of expressions whose list of values is precisely a given list.
--
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l       <- exprs ls,
                r       <- exprs rs,
                e       <- combine l r]

-- Combine expressions using each of the four numeric operators.
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- Return all possible expressions that solve an instance of the countdown problem.
-- Brute force approach.
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- --- Improved algorithm ---

type Result = (Expr, Int)

-- Return possible results. Filter invalid expressions.
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx       <- results ls,
                    ry       <- results rs,
                    res      <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

