module AbstractMachine where


-- --------------------------------------------------------------
-- ---------------- Example: Abstract Machine -------------------
-- --------------------------------------------------------------

-- Simple arithmetic expression evaluator limited to integer addition.

-- Expression model:
data Expr = Val Int
          | Add Expr Expr
          -- | Mul Expr Expr

-- Function to evaluate such an expression to an integer value:
value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
-- value (Mul x y) = value x * value y

-- Control stack with list of operations to perform.
type Cont = [Op]

data Op = EVAL Expr
        | ADD Int
        -- | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
-- eval (Mul x y) c = eval x (MUL EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
-- exec (MUL n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval e []
