module Main where

import System.Environment (getArgs)

import Morse (translate, printMorseCode, createSoundSamples)
import Sound (play)


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Morse Coder"
    if null args then putStrLn "Message required as argument."
        else
                do
                    let tokens = translate (head' args)
                    printMorseCode tokens
                    play $ createSoundSamples [] tokens
                    where
                        head' [] = error "arment missing."
                        head' (x:xs) = x

