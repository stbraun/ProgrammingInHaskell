module Sound where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import Text.Printf
import System.Process

volume :: Float
volume = 0.5

sampleRate :: Float
sampleRate = 48000

pitchStandard :: Frequency
pitchStandard = 440

outputFilePath :: FilePath
outputFilePath = "sound.bin"

-- Duration in seconds.
type Duration = Float

-- Frequency in Hz.
type Frequency = Float

-- The samples for a single tone.
type Sample = Float

type Semitones = Float


f :: Semitones -> Frequency
f n = pitchStandard * (2**(1.0/12.0))**n


-- Create samples for a note n semi tones from pitch standard.
note :: Semitones -> Duration -> [Sample]
note n duration = tone (f n) duration


-- Create samples for a tone of given frequency and duration.
tone :: Frequency -> Duration -> [Sample]
tone frequency duration = map (* volume) $ map sin $ map (* step) [0.0..sampleRate * duration]
    where
        step = (frequency * 2 * pi) / sampleRate


save :: FilePath -> [Sample] -> IO ()
save filePath samples = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE samples


play :: [Sample] -> IO ()
play samples = do
    save outputFilePath samples
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()


-- Compose a series of tones.
testWave :: [Sample]
testWave = concat [ note 0 0.5
              , note 2 0.25
              , note 4 0.25
              , note 4 0.5
              ]

