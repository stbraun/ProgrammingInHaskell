module Life where

import Control.Concurrent

import TerminalIO (Pos, cls, writeat, goto)

-- Game of Life
--

-- Width and height of the game board.
width :: Int
width = 50

height :: Int
height = 25

-- Delay between two generations in µs.
delay :: Int
delay = 75000

-- The board is represented as a list of positions at which there is a living cell.
type Board = [Pos]

-- Example of a board
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


-- Display a board with living cells on the screen.
showcells :: Board -> IO ()
showcells b= sequence_ [writeat p "O" | p <- b]

-- Check if cell at given position is alive.
isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

-- Check if cell at given position is empty.
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- Get the neighbors of a position.
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                           (x+1,y-1), (x-1,y),
                           (x+1,y), (x-1,y+1),
                           (x,y+1), (x+1,y+1)]

-- Take account of the wrapping around at the edges of the board.
wrap:: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) +1)


-- Calculate the number of live neighbors.
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- Calculate list of survivors.
survivors :: Board -> [Pos]
survivors b= [p | p <- b, liveneighbs b p `elem` [2,3]]

-- Calculate new born cells.
-- Consider only neighbors of living cells for efficiency.
births :: Board -> [Pos]
births b = [p | p <- rmdups (concatMap neighbs b),
                isEmpty b p,
                liveneighbs b p == 3]

-- Remove duplicate elements from a list.
rmdups :: Eq a => [a] ->[a]
rmdups [] = []
rmdups (x:xs) = x : rmdups ( filter (/= x) xs)

-- Calculate the next generation of a board.
nextgen :: Board -> Board
nextgen b = survivors b ++ births b

-- Run the game of life
life :: Board -> IO ()
life b = do
            cls
            showcells b
            threadDelay delay
            life (nextgen b)

