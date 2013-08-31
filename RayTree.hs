module RayTree where

import Ray
import Primitive
import Vec3
import Material

import Data.Maybe
import Data.List

volumeThreshold = 0.001

trace :: Ray -> [Primitive] -> [Ray]
trace ray primitives    | c == Nothing  = ray : []
                        | otherwise     = ray : trace newRay primitives
    where   c           = closestPrimitive ray primitives
            newRay      = reflected ray $ d c
            d (Just a)  = a

reflected ray primitive = Ray pos dir vol
    where   pos     = ((Vec3 dist dist dist) * (direction ray)) + (position ray)
            dist    = fromMaybe 0 $ intersection ray primitive
            dir     = reflect nor (direction ray)
            nor     = findNormal pos primitive
            vol     = (volume ray) * (reflective $ material $ primitive)

closestPrimitive :: Ray -> [Primitive] -> Maybe Primitive
closestPrimitive ray primitives     | inter == []   = Nothing
                                    | otherwise     = Just $ snd $ minimumBy (\(Just a, _) (Just b, _) -> compare a b) inter
    where   pairs   = zip (map (intersection ray) primitives) primitives
            inter   = filter (\(d, _) -> isJust d) pairs
