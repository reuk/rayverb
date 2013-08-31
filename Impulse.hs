module Impulse where

data Impulse = Impulse {
    samplePosition  :: Int,
    amplitude       :: Double
} deriving (Eq, Show)

sampleRate :: Double
sampleRate = 44100

raySpeed :: Double
raySpeed = 340 / sampleRate

getSources :: [Primitive] -> [Primitive]
getSources = filter (\p -> isSource p)

