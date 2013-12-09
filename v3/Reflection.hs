module Reflection where

import Ray
import Primitive
import Vec3
import Material
import Mic
import Container

import Data.Maybe
import Data.List hiding (fmap, foldr, all, minimumBy)

import Prelude hiding (fmap, foldr, all)

import Data.Foldable

import Control.Applicative

data Reflection = Reflection {
    surface     :: Surface,
    point       :: Vec3 Double,
    norm        :: Vec3 Double,
    refDist     :: Double,
    refVol      :: C3 Double
} deriving (Eq, Show)

trace :: [Primitive] -> Ray -> Double -> C3 Double -> [Reflection]
trace primitives ray dist vols                  | all (< vt) vols               = []    --  can I do this with mapping?
                                                | c == Nothing                  = []
                                                | isSource $ f                  = ref : []
                                                | otherwise                     = ref : trace primitives newRay newDist newVols
    where   ref                                 = Reflection (material f) p (findNormal p f) newDist newVols
            c                                   = closestPrimitive ray primitives
            d (Just a)                          = a
            f                                   = d c
            p                                   = position newRay
            newRay                              = reflected ray f
            newVols                             = liftA2 (\ x y -> x * reflective y) vols $ material f
            newDist                             = dist + (difference (position ray) p)
            vt                                  = 0.001

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