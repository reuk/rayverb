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
    point       :: Vec3 Double
} deriving (Eq, Show)

trace :: [Primitive] -> Ray -> [Reflection]
trace primitives ray    | volume ray < vt   = []
                        | c == Nothing      = []
                        | isSource $ f      = ref : []
                        | otherwise         = ref : trace primitives newRay
    where   ref         = Reflection (material f) (position newRay)
            c           = closestPrimitive ray primitives
            newRay      = reflected ray f
            f           = d c
            d (Just a)  = a
            vt          = 0.001

reflected ray primitive = Ray pos dir vol
    where   pos     = ((Vec3 dist dist dist) * (direction ray)) + position ray
            dist    = fromMaybe 0 $ intersection ray primitive
            dir     = reflect nor (direction ray)
            nor     = findNormal pos primitive
            vol     = (volume ray) * (reflective $ material $ primitive)

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
createRay mic (r, phi) = Ray (location mic) dir (getAttenuation mic dir)
    where   dir = unitVec3 r phi

createRays :: Vardioid -> Int -> [Ray]
createRays mic number = map (createRay mic) [(r i, phi j) | i <- [0 .. flim], j <- [0 .. flim]]
    where   lim     = sqrt $ fromIntegral number
            flim    = floor lim
            r x     = (fromIntegral (x * 2) - lim) / lim
            phi x   = (fromIntegral x * 2 * pi) / lim

traceMic :: [Primitive] -> Int -> Vardioid -> [[Reflection]]
traceMic primitives number mic = map (trace primitives) $ createRays mic number

traceMics :: [Primitive] -> Int -> [Vardioid] -> [[[Reflection]]]
traceMics primitives number = map (traceMic primitives number)
