module Parser where

import Control.Applicative
import Data.Char


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


