module Nim where

import Data.Char

-- This variant of the Game of Nim is played on a board comprising five numbered rows of stars,
-- initially set as follows:
--
-- 1: *****
-- 2: ****
-- 3: ***
-- 4: **
-- 5: *
--
-- Two players take it in turn to remove one or more stars from the end of a single row.
-- The winner is the player who maes the board empty, that is,
-- who removes the final star or stars from the board.

-- Players are represented as numbers 1 and 2.

-- next gives the next player.
next :: Int -> Int
next 1 = 2
next 2 = 1
next n = error "invalid player"

-- The board is represented asa list comprising thenmber of stars that remain on each row.
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]


main :: IO ()
main = nim


-- A game is being finished when all rows have no stars left.
finished :: Board -> Bool
finished = all (== 0)

-- A move is specified by a row number and the number of stars tobe removed,
-- and is valid if the row contains at least this many stars.
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

-- move applies a move to a board.
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                        where update r n = if r== row then n-num else n

-- Dislay a row of the board on the screen.
putRow :: Int -> Int -> IO ()
putRow row num = do
                    putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- Display the board on the screen.
putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow n s | (n,s) <- zip [1..] b]

-- Get a digit from the keyboard.
getDigit :: String -> IO Int
getDigit prompt = do
                    putStr prompt
                    x <- getChar
                    newline
                    if isDigit x then
                        return (digitToInt x)
                    else
                        do
                            putStrLn "ERROR: Invalid digit"
                            getDigit prompt

-- Move onto a new line.
newline :: IO ()
newline = putChar '\n'

-- The main game loop.
play :: Board -> Int -> IO()
play board player = do
                        putBoard board
                        if finished board then
                            do
                                newline
                                putStr "Player "
                                putStr (show (next player))
                                putStrLn " wins!"
                        else
                            do
                                newline
                                putStr "Player "
                                print player
                                row <- getDigit "Enter a row number: "
                                num <- getDigit "Stars to remove: "
                                if valid board row num then
                                    play (move board row num) (next player)
                                else
                                    do
                                        newline
                                        putStrLn "ERROR: Invalid move"
                                        play board player

-- Start the game.
nim :: IO ()
nim = play initial 1

