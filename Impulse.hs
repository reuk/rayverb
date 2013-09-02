module Impulse where

import Reflection
import Vec3
import Primitive
import Mic
import Material

data Impulse = Impulse {
    samplePosition  :: Int,
    amplitude       :: Double
} deriving (Eq, Show)

sampleRate :: Double
sampleRate = 44100

samplesPerUnit :: Double
samplesPerUnit = sampleRate / 340

getSources :: [Primitive] -> [Primitive]
getSources = filter (\p -> isSource p)

toImpulse :: Primitive -> Reflection -> Impulse
toImpulse s r = Impulse p a
    where   p           = floor (samplesPerUnit * d)
            d           = refDist r + difference (point r) (origin s) - (radius s)
            a           = (refVol r) * (dot (norm r) toSource) * (diffuse $ surface r)
            toSource    = normalize ((origin s) - (point r))

toImpulses :: Primitive -> [Reflection] -> [Impulse]
toImpulses s r = map (toImpulse s) r
