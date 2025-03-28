module TailRec where
-- | Tail-recursive functions

-- | Fibonacci function using tail recursion
fib :: Integer -> Integer
fib n = go n 0 1
  where
    go 0 a _ = a
    go 1 _ b = b
    go n a b = go (n-1) b (a+b)

-- | Factorial function using tail recursion
fact :: Integer -> Integer
fact n = go n 1
  where
    go 0 a = a
    go n a = go (n-1) (n*a)

