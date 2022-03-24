module Parser where

import Control.Applicative
import Data.Char

import Lib (writeat, getCh, beep, goto, cls)


newtype Parser a = P (String -> [(a,String)])


-- Applies a parser to a string, removing the dummy constructor P.
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- A parsing primitivr, which fails for an empty input string,
-- and succeeds with the first character as the result value otherwise.
item :: Parser Char
item = P (\inp -> case inp of
        [] -> []
        (x:xs) -> [(x,xs)])


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> [(g v, out)])


instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser a
    pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g,out)] -> parse (fmap g px) out)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> parse (f v) out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|> :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [(v,out)])


-- A parser for single characters that satisfy the predicate p.
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty


-- A set of parsers based on sat.

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)


-- A parser for given string.
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


-- A parser for identifiers, starting with a lowercase letter.
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)


-- A parser for natural numbers.
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)


-- A parser for integers.
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat


-- A parser which silently removes white spaces.
space :: Parser ()
space = do many (sat isSpace)
           return ()


-- A primitive that ignores spaces before and after applying a parser for a token.
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v


-- Some parsers that ignore white space.

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int


-- A parser for the given symbol.
symbol :: String -> Parser String
symbol xs = token (string xs)


-- A parser for a non-empty list of natural numbers.
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","; natural)
          symbol "]"
          return (n:ns)


-- ------ Simple expression parser -------

-- expr   ::= term ( + expr | - expr | eps)
-- term   ::= factor ( * term | / term | eps)
-- factor ::= ( expr ) | int
-- int    ::= ... | -2 | -1| 0 | 1 | 2 | ...

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            <|> do symbol "-"
                   e <- expr
                   return (t - e)
                  <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            <|> do symbol "/"
                   t <- term
                   return (f `div` t)
                  <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           <|> integer

eval' :: String -> Int
eval' xs = case (parse expr xs) of
    [(n,[])]  -> n
    [(_,out)] -> error ("Unused input " ++ out)
    []        -> error "Invalid input"

-- ------ Simple calculator ------

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = +",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

-- Define supported buttons. Besides the buttons appearing on the box
-- a number of extra characters will be allowed for flexibility.
buttons :: String
buttons = standard ++ extra
    where
        standard = "qcd=123+456-789*0()/"
        extra = "QCD \ESC\BS\DEL\n"

-- Display the calculator box.
showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

-- Show a string in the display of the calculator.
-- First clears the display, then shows tha last thirteen characters of the string.
display :: String -> IO ()
display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

-- The calculator core function.
calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

-- Perform action on current string.
process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

-- Set cursor below calculator and terminate.
quit :: IO ()
quit = goto (1,14)

-- Remove the last character.
delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

-- Evaluate the current expression.
eval :: String -> IO ()
eval xs = case parse expr xs of
    [(n, [])] -> calc (show n)
    _         -> do beep
                    calc xs

-- Clear the display.
clear :: IO ()
clear = calc []

-- Append character to end of input string.
press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

-- Run the calculator
calculator :: IO ()
calculator = do cls
                showbox
                clear

