module Hangman where

import System.IO

import TerminalIO (getCh)


main :: IO ()
main = do
        hangman


hangman :: IO ()
hangman = do
            putStrLn "Think of a word:"
            word <- sgetLine
            putStrLn "Try to guess it:"
            play word


-- Get line from keyboard but echo each character as a dash symbol '-'.
sgetLine :: IO String
sgetLine = do
            x <- getCh
            if x == '\n' then
                do
                    putChar x
                    return []
            else
                do
                    putChar '_'
                    xs <- sgetLine
                    return (x:xs)


play :: String -> IO ()
play word = do
                putStr "? "
                guess <- getLine
                if guess == word then
                    putStrLn "You got it!"
                else
                    do
                        putStrLn (match word guess)
                        play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

