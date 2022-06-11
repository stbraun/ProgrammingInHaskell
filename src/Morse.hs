-- |
-- Morse Translator.
--
-- 1. The length of a dot is one unit.
-- 2. A dash is three units.
-- 3. The space between parts of the same letter is one unit.
-- 4. The space between letters is three units.
-- 5. The space between words is seven units.
--

module Morse where

import qualified Data.Map as Map

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


-- |
-- Translate a string into a list of morse tokens.
translate :: String -> Maybe [Token]
translate "" = Just []
translate (c:rest) = pure (++) <*> mapCharToToken c <*> (translate rest)
    where
        mapCharToToken :: Char -> Maybe [Token]
        mapCharToToken c = Map.lookup c alphabet


