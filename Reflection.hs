module Reflection where

import Ray
import Primitive
import Vec3
import Material
import Mic

import Data.Maybe
import Data.List

data Reflection = Reflection {
    surface     :: Material,
    point       :: Vec3 Double,
    norm        :: Vec3 Double,
    refDist     :: Double,
    refVol      :: Double
} deriving (Eq, Show)

trace :: [Primitive] -> Double -> Double -> Ray -> [Reflection]
trace primitives dist vol ray   | vol < vt      = []
                                | c == Nothing  = []
                                | isSource $ f  = ref : []
                                | otherwise     = ref : trace primitives newDist newVol newRay
    where   ref                 = Reflection (material f) p (findNormal p f) newDist newVol
            c                   = closestPrimitive ray primitives
            d (Just a)          = a
            f                   = d c
            p                   = position newRay
            newRay              = reflected ray f
            newVol              = vol * (reflective $ material f)
            newDist             = dist + (difference (position ray) p)
            vt                  = 0.001

reflected ray primitive = Ray pos dir 
    where   pos     = ((Vec3 dist dist dist) * (direction ray)) + position ray
            dist    = fromMaybe 0 $ intersection ray primitive
            dir     = reflect nor (direction ray)
            nor     = findNormal pos primitive

closestPrimitive :: Ray -> [Primitive] -> Maybe Primitive
closestPrimitive ray primitives     | inter == []   = Nothing
                                    | otherwise     = Just $ snd $ minimumBy cjt inter
    where   cjt (Just a, _) (Just b, _)     = compare a b
            pairs                           = zip (map (intersection ray) primitives) primitives
            inter                           = filter (\(d, _) -> isJust d) pairs

unitVec3 :: Double -> Double -> Vec3 Double
unitVec3 r phi = Vec3 (z2 * cos phi) (z2 * sin phi) r
    where   z2  = sqrt (1 - (r * r))

createRay :: Vardioid -> (Double, Double) -> Ray
createRay mic (r, phi) = Ray (location mic) dir 
    where   dir = unitVec3 r phi

createRays :: Vardioid -> Int -> [Ray]
createRays mic number = map (createRay mic) [(r i, phi j) | i <- [0 .. flim], j <- [0 .. flim]]
    where   lim     = sqrt $ fromIntegral number
            flim    = floor lim
            r x     = (fromIntegral (x * 2) - lim) / lim
            phi x   = (fromIntegral x * 2 * pi) / lim

traceMic :: [Primitive] -> Int -> Vardioid -> [[Reflection]]
traceMic primitives number mic = map f (createRays mic number)
    where   f x = trace primitives 0 (getAttenuation mic (direction x)) x

traceMics :: [Primitive] -> Int -> [Vardioid] -> [[[Reflection]]]
traceMics primitives number = map (traceMic primitives number)
