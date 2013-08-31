module Impulse where

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

toImpulses :: [Reflection] -> Double -> Double -> [Impulse]
toImpulses [x] dist vol         = (Impulse (floor (dist * samplesPerUnit)) vol) : []
toImpulses (x:y:xs) dist vol    = dif : toImpulses xs (dist + d) (vol * v) 
    where   dif     = -- calculate diffuse impulse here
            d       = difference (point x) (point y)
            v       = reflective $ material $ x
            
impulseResponse :: [Primitive] -> Int -> Vardioid -> [[Impulse]]
impulseResponse primitives number mic = map (toImpulses dist vol) $ traceMic primitives number mic 
