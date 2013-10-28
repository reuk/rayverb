module RayTree where

import Ray
import Primitive
import Vec3
import Material
import Mic

import Data.Maybe
import Data.List

trace :: [Primitive] -> Ray -> [Ray]
trace primitives ray    | volume ray < vt   = ray : [] 
                        | c == Nothing      = ray : []
                        | isSource $ d c    = ray : []
                        | otherwise         = ray : trace primitives newRay
    where   c           = closestPrimitive ray primitives
            newRay      = reflected ray $ d c
            d (Just a)  = a
            vt          = 0.001

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

unitVec3 :: Double -> Double -> Vec3 Double
unitVec3 r phi = Vec3 (z2 * (cos phi)) (z2 * (sin phi)) r
    where   z2  = sqrt (1 - (r * r))

createRay :: Vardioid -> (Double, Double) -> Ray
createRay mic (r, phi) = Ray (location mic) dir (getAttenuation mic dir) 
    where   dir = unitVec3 r phi 

--createRays :: Vardioid -> Int -> [Ray]
--createRays mic number = map (createRay mic) $ take number $ zip (randomRs (-1, 1) (makeStdGen 11)) (randomRs (0, 2 * pi) (makeStdGen 19)) 

createRays :: Vardioid -> Int -> [Ray]
createRays mic number = map (createRay mic) [((fromIntegral (i * 2) - lim) / lim, (fromIntegral j * 2 * pi) / lim) | i <- [0 .. floor lim], j <- [0 .. floor lim]] 
    where   lim = sqrt $ fromIntegral number

traceMic :: [Primitive] -> Int -> Vardioid -> [[Ray]]
traceMic primitives number mic = map (trace primitives) $ createRays mic number 

traceMics :: [Primitive] -> Int -> [Vardioid] -> [[[Ray]]]
traceMics primitives number = map (traceMic primitives number)
