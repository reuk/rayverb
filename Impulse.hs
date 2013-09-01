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

toImpulses :: [Reflection] -> Double -> Double -> Primitive -> [Impulse]
toImpulses [x] dist vol p       = (Impulse (floor (dist * samplesPerUnit)) vol) : []
toImpulses (x:y:xs) dist vol p  = dif : toImpulses xs (dist + d) (vol * v) p
    where   dif         = Impulse sp difVal 
            difVal      = vol * (dot (norm x) toSource) * (diffuse $ surface x)
            toSource    = normalize ((origin p) - (point x))
            d           = difference (point x) (point y)
            e           = difference (point x) (origin p) - radius p
            sp          = floor ((d + e) * samplesPerUnit)
            v           = reflective $ surface x
            
--impulseResponse :: [Primitive] -> Int -> Vardioid -> [[Impulse]]
--impulseResponse primitives number mic = map (toImpulses dist vol) $ traceMic primitives number mic
