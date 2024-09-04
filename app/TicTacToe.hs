module TicTacToe where

-- An interractive tic-tac-toe.

import Data.Char
import Data.List
import System.IO

import TerminalIO (cls, goto)

-- size of the grid
size :: Int
size = 3

-- depth of gametree
depth :: Int
depth = 9

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)


-- Give the next player. B is included for completeness even though
-- the function should never be applied to this value.
next :: Player -> Player
next O = X
next B = B
next X = O

-- Define the empty grid.
empty :: Grid
empty = replicate size (replicate size B)

-- Check for a full grid.
full :: Grid -> Bool
full = all (/= B) . concat

-- Decide whose player's turn it is.
turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
                os = length (filter (==O) ps)
                xs = length (filter (==X) ps)
                ps = concat g

-- Check if player won the game.
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
            line = all (== p)
            rows = g
            cols = transpose g
            dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

-- Check if either player has won.
won :: Grid -> Bool
won g = wins O g || wins X g

-- Display a grid
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

-- Display one row of a grid.
showRow :: [Player] -> [String]
showRow = beside . interleave bar .map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar    = replicate 3 "|"

-- Display a single player.
showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- Interleave a value between each element in a list.
interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [y]    = [y]
interleave x (y:ys) = y: x: interleave x ys

-- Check if a move is valid.
-- The grid cells are indexed starting from 0 at the top left increasing to the bottom right.
-- A move must address a valid index and a blank cell.
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

-- Apply a move to a grid.
-- An empty list as result denotes failure, a singleton list denotes success.
move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop size (xs ++ [p] ++ ys) | valid g i]
             where (xs,B:ys) = splitAt i (concat g)

-- Undo a concat on a grid
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

--  Read a number.
getNat :: String -> IO Int
getNat msg = do
                    putStr msg
                    xs <- getLine
                    if xs /= [] && all isDigit xs then
                        return (read xs)
                    else
                        do
                            putStrLn "ERRO: Invalid number"
                            getNat msg


-- --------------------------------------------------------------------
-- ----- Human vs. human game. -----
-- --------------------------------------------------------------------
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
            cls
            goto (1,1)
            putGrid g
            run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
                do
                    i <- getNat (prompt p)
                    case move g i p of
                        [] -> do
                                putStrLn "ERROR: Invalid move"
                                run' g p
                        [g'] -> run g' (next p)
                        _    -> error "unexpected move"

-- Prompt player for next move.
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


-- --------------------------------------------------------------------
-- ----- Computer player -----
-- --------------------------------------------------------------------

data Tree a = Node a [Tree a]
    deriving Show

-- Build a game tree.
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- Return the grids resulting from all valid moves.
moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- Prune the game tree to a given depth in order to limit resource usage.
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Minimax algorithm labels the gametree.
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
    | wins O g  = Node (g, O) []
    | wins X g  = Node (g, X) []
    | otherwise = Node (g, B) []
minimax (Node g ts)
    | turn g == O = Node (g, minimum ps) ts'
    | turn g == X = Node (g, maximum ps) ts'
                    where
                        ts' = map minimax ts
                        ps = [p | Node (_, p) _ <- ts']
minimax _ = error "unexpexted call"

-- Get the best move for the player. This is given by the node below the root node
-- that has the same label as the root node, i.e. the label of the current player.
bestmove :: Grid -> Player -> Grid
bestmove g p = head' [g' | Node (g', p') _ <- ts, p' == best]
                where
                    tree = prune depth (gametree g p)
                    Node (_, best) ts = minimax tree
                    head' [] = error "No best move found."
                    head' (x:xs) = x

-- Main entry point.
main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        p <- firstPlayer
        play empty p

-- Ask user which player to start the game.
firstPlayer :: IO Player
firstPlayer = do
                putStr "Do you want to take the first move? (y/n) "
                c <- getLine
                putStrLn ""
                case c of
                    "y" -> return O
                    "n" -> return X
                    _   -> do
                            putStrLn "Please enter y or n."
                            firstPlayer

-- Play human vs computer.
play :: Grid -> Player -> IO ()
play g p = do
            cls
            goto (1,1)
            putGrid g
            play' g p

play' :: Grid -> Player -> IO ()
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do
                    i <- getNat (prompt p)
                    case move g i p of
                        []   -> do
                                    putStrLn "ERROR: Invalid move"
                                    play' g p
                        [g'] -> play g' (next p)
                        _    -> error "unexpected move"
    | p == X   = do
                    putStr "Player X is thinking..."
                    (play $! bestmove g p) (next p)
play' _ _ = error "unexpected call"

