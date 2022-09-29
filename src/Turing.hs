module Turing (
        runTuring
    ) where


data State = Start | Even | Odd | Halt
    deriving (Ord, Eq, Show)

-- | Simple Turing machine.
-- Takes a 'band' preset with 01111...1110.
-- Determines if the number of 1s is even or odd.
-- Prints 0 for even, 1 for odd number of 1s.
runTuring :: String -> IO ()
runTuring band = turing band Start


turing :: String -> State -> IO ()
turing [] _ = do
        invalid Nothing
        return ()
turing band Halt = do
        putStrLn band
        return ()
turing ('1':xs) Start = do
        invalid (Just $ "Invalid data: " <> ('1':xs) <> ".")
        return ()
turing ('0':xs) Start = turing xs Even
turing ('0':xs) Even = turing "0" Halt
turing ('1':xs) Even = turing xs Odd
turing ('0':xs) Odd = turing "1" Halt
turing ('1':xs) Odd = turing xs Even
turing (x:xs) _ = invalid $ Just $ "Unexpected char '" <> [x] <> "'."

-- | Print error message for invalid argument.
-- Takes an optional message prefix.
invalid :: Maybe String -> IO ()
invalid (Just prefix) = putStrLn $ prefix <> " " <> expected
invalid Nothing = putStrLn expected

-- | Help message for expected argument.
expected :: String
expected = "Expected a string starting with 0, sequence of 1, and a trailing 0, e.g., 01110."

