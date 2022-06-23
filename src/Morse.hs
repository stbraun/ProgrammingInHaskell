-- |
-- Morse Translator.
--
-- 1. The length of a dot is one unit.
-- 2. A dash is three units.
-- 3. The space between parts of the same letter is one unit.
-- 4. The space between letters is three units.
-- 5. The space between words is seven units.
--

module Morse
    ( translate
    , printMorseCode
    , createSoundSamples
    ) where

import qualified Data.Map as Map
import Data.Char (toUpper)

import Sound


-- |
-- Tokens of a morse stream.
data Token = Dot | Sep | Dash | LetterSep | WordSep
    deriving (Show, Eq, Ord)


-- |
-- Morse encoding.
alphabet :: Map.Map Char [Token]
alphabet = Map.fromList [
              ('A', [Dot ,Sep, Dash, LetterSep])
            , ('B', [Dash ,Sep, Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('C', [Dash ,Sep, Dot ,Sep, Dash ,Sep, Dot, LetterSep])
            , ('D', [Dash ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('E', [Dot, LetterSep])
            , ('F', [Dot ,Sep, Dot ,Sep, Dash ,Sep, Dot, LetterSep])
            , ('G', [Dash ,Sep, Dash ,Sep, Dot, LetterSep])
            , ('H', [Dot ,Sep, Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('I', [Dot ,Sep, Dot, LetterSep])
            , ('J', [Dot ,Sep, Dash ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('K', [Dash ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('L', [Dot ,Sep, Dash ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('M', [Dash ,Sep, Dash, LetterSep])
            , ('N', [Dash ,Sep, Dot, LetterSep])
            , ('O', [Dash ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('P', [Dot ,Sep, Dash ,Sep, Dash ,Sep, Dot, LetterSep])
            , ('Q', [Dash ,Sep, Dash ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('R', [Dot ,Sep, Dash ,Sep, Dot, LetterSep])
            , ('S', [Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('T', [Dash, LetterSep])
            , ('U', [Dot ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('V', [Dot ,Sep, Dot ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('W', [Dot ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('X', [Dash ,Sep, Dot ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('Y', [Dash ,Sep, Dot ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('Z', [Dash ,Sep, Dash ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('0', [Dash ,Sep, Dash ,Sep, Dash ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('1', [Dot ,Sep, Dash ,Sep, Dash ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('2', [Dot ,Sep, Dot ,Sep, Dash ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('3', [Dot ,Sep, Dot ,Sep, Dot ,Sep, Dash ,Sep, Dash, LetterSep])
            , ('4', [Dot ,Sep, Dot ,Sep, Dot ,Sep, Dot ,Sep, Dash, LetterSep])
            , ('5', [Dot ,Sep, Dot ,Sep, Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('6', [Dash ,Sep, Dot ,Sep, Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('7', [Dash ,Sep, Dash ,Sep, Dot ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('8', [Dash ,Sep, Dash ,Sep, Dash ,Sep, Dot ,Sep, Dot, LetterSep])
            , ('9', [Dash ,Sep, Dash ,Sep, Dash ,Sep, Dash ,Sep, Dot, LetterSep])
            , (' ', [WordSep])
        ]


-- |
-- Duration of tokens in units.
-- Note: word separator has a duration of seven units,
-- but because it follows letter separator it is encoded with four units only.
durations = Map.fromList [
              (Dot, 1)
            , (Sep, 1)
            , (Dash, 3)
            , (LetterSep, 3)
            , (WordSep, 4)  -- = 7 - LetterSep
            ]

getDurationFactor :: Token -> Int
getDurationFactor t = convert $ Map.lookup t durations
    where
        convert (Just d) = d
        convert Nothing = 0


-- |
-- Translate a string into a list of morse tokens.
translate :: String -> Either String [Token]
translate "" = Right []
translate (c:rest) = pure (++) <*> mapCharToToken c <*> (translate rest)
    where
        mapCharToToken :: Char -> Either String [Token]
        mapCharToToken c = convert (Map.lookup (toUpper c) alphabet)
            where
                convert :: Maybe [Token] -> Either String [Token]
                convert Nothing = Left ("Unknown character: " ++ [c])
                convert (Just ts) = Right ts


-- |
-- Print morse code.
printMorseCode :: Either String [Token] -> IO ()
printMorseCode (Left msg) = putStrLn msg
printMorseCode (Right []) = putStrLn "stop"
printMorseCode (Right (t:ts)) = do
        printToken t
        printMorseCode (Right ts)
    where
        printToken t
            | t == Dot = putStr "o"
            | t == Dash = putStr "---"
            | t == Sep = putStr "."
            | t == LetterSep = putStr "..."
            | t == WordSep = putStrLn "...."


-- Generate audio signal.

frequency :: Frequency
frequency = pitchStandard * 2

silence :: Frequency
silence = 0.0

dotDuration :: Duration
dotDuration = 0.1

scaleDuration :: Token -> Duration
scaleDuration token = dotDuration * (fromIntegral $ getDurationFactor token)

dashDuration = scaleDuration Dash
sepDuration = scaleDuration Sep
letterSepDuration = scaleDuration LetterSep
wordSepDuration = scaleDuration WordSep


dotSamples :: [Sample]
dotSamples = tone frequency dotDuration

dashSamples :: [Sample]
dashSamples = tone frequency dashDuration

sepSamples = tone silence sepDuration
letterSepSamples = tone silence letterSepDuration
wordSepSamples = tone silence wordSepDuration


createSoundSamples :: [Sample] -> Either String [Token] -> [Sample]
createSoundSamples samples (Left _) = samples
createSoundSamples samples (Right []) = samples
createSoundSamples samples (Right (t:ts)) = do
        createSoundSamples (addSamples t) (Right ts)
    where
        addSamples t
            | t == Dot = samples ++ dotSamples
            | t == Dash = samples ++ dashSamples
            | t == Sep =  samples ++ sepSamples
            | t == LetterSep = samples ++ letterSepSamples
            | t == WordSep = samples ++ wordSepSamples

