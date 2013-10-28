module Impulse where

import Reflection
import Vec3
import Primitive
import Mic
import Material

import Data.List

data Impulse = Impulse {
    samplePosition  :: Int,
    amplitude       :: Double
} deriving (Eq, Show)

sampleRate :: Int
sampleRate = 44100

samplesPerUnit :: Double
samplesPerUnit = (fromIntegral sampleRate) / 340

getSources :: [Primitive] -> [Primitive]
getSources = filter (\p -> isSource p)

toImpulse :: Primitive -> Reflection -> Impulse
toImpulse s r = Impulse p a
    where   p           = floor (samplesPerUnit * d)
            d           = refDist r + difference (point r) (origin s) - (radius s)
            a           = (refVol r) * (dot (norm r) toSource) * (diffuse $ surface r)
            na          = ((power s) * a) / (d * d)
            nna         = ((power s) * a) / d
            toSource    = normalize ((origin s) - (point r))

toImpulses :: Primitive -> [Reflection] -> [Impulse]
toImpulses s r = map (toImpulse s) r

allSourceImpulses :: [Primitive] -> [Reflection] -> [Impulse]
allSourceImpulses s r = concat $ map (\x -> toImpulses x r) s

fullTrace :: [[[Reflection]]] -> [Primitive] -> [Impulse]
fullTrace r p = allSourceImpulses p (concat $ concat r)

sortImpulses :: [Impulse] -> [Impulse]
sortImpulses = sortBy (\x y -> compare (samplePosition x) (samplePosition y))

convert :: Int -> [Impulse] -> [Double]
convert lastPos []       = []
convert lastPos impulses = z ++ [s] ++ convert p d
    where   i   = takeWhile f impulses
            d   = dropWhile f impulses
            f x = samplePosition x == p
            p   = samplePosition $ head impulses
            s   = sum $ map amplitude i
            z   = take (lastPos - p - 1) $ repeat 0

correctVolume :: [Double] -> [Double]
correctVolume d = map (*x) d
    where   x   = 1 / (maximum d)

normalizeMics :: [[Double]] -> [[Double]]
normalizeMics d = map (map (*x)) d
    where   x   = 1 / (maximum $ concat d)

samples :: [Impulse] -> [Double]
samples = correctVolume . convert 0 . sortImpulses

lengthPad :: [[Double]] -> [[Double]]
lengthPad d = map (pad x) d
    where   x   = maximum $ map (length) d

pad :: Int -> [Double] -> [Double]
pad n l = l ++ (take (n - (length l)) $ repeat 0)

multiSamples :: [[Impulse]] -> [[Double]]
multiSamples = lengthPad . normalizeMics . map (convert 0 . sortImpulses)

--samples :: [Impulse] -> [Double]
--samples = correctVolume . convert []
--
--convert :: [Double] -> [Impulse] -> [Double]
--convert current []      = current
--convert current (x:xs)  | spx > l       = convert (current ++ z ++ [ax]) xs
--                        | otherwise     = convert a xs
--    where   z       = take (spx - l - 1) $ repeat 0
--            spx     = samplePosition x
--            ax      = amplitude x
--            l       = length current
--            na      = ax + (current !! spx)
--            (b, c)  = splitAt (spx - 1) current
--            a       = b ++ [na] ++ (tail c)
