module TerminalIO where

import System.IO


-- Position of a character on the screen.
type Pos = (Int,Int)


-- Clear screen.
cls :: IO ()
cls = putStr "\ESC[2J"


-- Write a string at a given position.
writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs


-- Move to given position.
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++"H")


-- Get char from keyboard without echoing it to the screen.
getCh:: IO Char
getCh = do
            hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

-- Ring the bell.
beep :: IO ()
beep = putStr "\BEL"

