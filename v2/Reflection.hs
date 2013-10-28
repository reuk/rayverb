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

reflected :: Ray -> Primitive -> Ray
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