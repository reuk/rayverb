module Primitive where

import Vec3
import Material
import Ray

data Primitive = Sphere {
    material    :: Material,
    isSource    :: Bool,
    origin      :: Vec3 Double,
    radius      :: Double
} | Plane {
    material    :: Material,
    isSource    :: Bool,
    normal      :: Vec3 Double,
    distance    :: Double
} deriving (Eq, Show)

intersection :: Ray -> Primitive -> Maybe Double
intersection ray (Sphere {origin = o, radius = r})
    | intersects && i1 < 0  = Just i2
    | intersects && i1 > 0  = Just i1
    | otherwise             = Nothing
    where   intersects      = determinant > 0 && i2 > 0
            i1              = b - sqdet
            i2              = b + sqdet
            determinant     = (b * b) - (dot v v) + (r * r)
            sqdet           = sqrt determinant
            b               = - dot v (direction ray)
            v               = (position ray) - o

intersection ray (Plane {normal = n, distance = d})
    | intersects    = Just e
    | otherwise     = Nothing
    where   intersects  = det /= 0 && e > 0
            det         = dot n (direction ray)
            e           = (d - dot n (position ray)) / det

findNormal :: Vec3 Double -> Primitive -> Vec3 Double
findNormal pos (Sphere {origin = o}) = normalize (pos - o)
findNormal pos (Plane {normal = n}) = n
