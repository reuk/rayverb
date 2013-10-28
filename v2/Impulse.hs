module Impulse where

import Reflection
import Vec3
import Primitive
import Mic
import Material
import Ray

import Data.List

import System.Random

data Impulse = Impulse {
    samplePosition  :: Int,
    amplitude       :: Double
} deriving (Eq, Show)

sampleRate :: Int
sampleRate = 44100

speedOfSound :: Double
speedOfSound = 340

samplesPerUnit :: Double
samplesPerUnit = (fromIntegral sampleRate) / speedOfSound

getSources :: [Primitive] -> [Primitive]
getSources = filter isSource

toImpulse :: Primitive -> Reflection -> Impulse
toImpulse s r = Impulse p a
    where   p           = floor (samplesPerUnit * d)
            d           = refDist r + difference (point r) (origin s) - (radius s)
            a           = (refVol r) * (dot (norm r) toSource) * (diffuse $ surface r)
            nna         = ((power s) * a) / d
            na          = nna / d
            toSource    = normalize ((origin s) - (point r))

toImpulsesAllSources :: [Primitive] -> Reflection -> [Impulse]
toImpulsesAllSources s r = map (\ x -> toImpulse x r) s

toImpulsesAllPrimitives :: [Primitive] -> Reflection -> [Impulse]
toImpulsesAllPrimitives = toImpulsesAllSources . getSources

unitVec3 :: Double -> Double -> Vec3 Double
unitVec3 r phi = Vec3 (z2 * cos phi) (z2 * sin phi) r
    where   z2  = sqrt (1 - (r * r))

createRay :: Mic -> (Double, Double) -> Ray
createRay mic (r, phi) = Ray (location mic) dir 
    where   dir = unitVec3 r phi

createRays :: Mic -> Int -> [Ray]
createRays mic number = map (createRay mic) $ take number $ zip (randomRs (-1, 1) (mkStdGen 11)) (randomRs (0, 2 * pi) (mkStdGen 19))

traceToImpulse :: [Primitive] -> Ray -> [Impulse]
traceToImpulse primitives ray = concat $ map (toImpulsesAllPrimitives primitives) (trace primitives 0 1 ray)

traceDirection :: [Primitive] -> Ray -> (Vec3 Double, [Impulse])
traceDirection primitives ray = (direction ray, traceToImpulse primitives ray)

traceMic :: [Primitive] -> Mic -> Int -> [(Vec3 Double, [Impulse])]
traceMic primitives mic number = map (traceDirection primitives) (createRays mic number)
