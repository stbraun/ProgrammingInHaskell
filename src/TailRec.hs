module TailRec where
-- | Tail-recursive functions

-- | Fibonacci function using tail recursion
fib :: Integer -> Integer
fib n = fib' n 0 1
  where
    -- | Helper function for fib taking two additional arguments
    -- | to store the previous two Fibonacci numbers.
    fib' :: Integer -> Integer -> Integer -> Integer
    fib' 0 a _ = a
    fib' 1 _ b = b
    fib' n a b = fib' (n-1) b (a+b)

-- | Factorial function using tail recursion
fact :: Integer -> Integer
fact n = fact' n 1
  where
    -- | Helper function for fact taking an additional argument
    -- | to store the current factorial value.
    fact' :: Integer -> Integer -> Integer
    fact' 0 acc = acc
    fact' n acc = fact' (n-1) (n*acc)

