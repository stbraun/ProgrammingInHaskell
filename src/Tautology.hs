module Tautology where

-- -------------------------------------------------------
-- ----- Tautology Checker
-- -------------------------------------------------------

-- Tautology: a logical expression that is always true.

-- Declare a type for propositions. It has one constructor
-- for each of the five forms that a proposition can have.
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving (Show)

-- Example propositions represented using Prop:
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Var 'B')

p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

p7 :: Prop
p7 = Equiv (Var 'A') (Var 'B')

-- For evaluation we need to associate the variables of a proposition
-- with ist value. We declare a substition as a lookup table that
-- associates variable names to logical values.
type Subst = Assoc Char Bool

-- Evaluate a proposition given a substitution for its variables.
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q   -- p=True, q =False -> True > False
eval s (Equiv p q) = eval s (Imply p q) && eval s (Imply q p)

-- Return a list of variables in a proposition.
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

-- Return a list of all combinations for given number of values.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)

-- Generate all possible lists of logical values for the number ov variables
-- in a proposition and zip the list of variables with
-- each of the resulting list, creating a substitution list.
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

-- Finally, we define a function that decides if a proposition is a tautology
-- by simply checking if it evaluates to True for all possible substitutions.
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


-- --- Utilities ---

-- Remove duplicates from a list.
rmdups :: Eq a => [a] ->[a]
rmdups [] = []
rmdups (x:xs) = x : rmdups ( filter (/= x) xs)

-- A type of lookup tables declared as list of key/value pairs.
type Assoc k v = [(k, v)]

-- A function to lookup a value in an associative list.
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
