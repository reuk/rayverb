module Primitive where

import Vec3
import Material
import Ray

data Primitive = Sphere {
    material    :: Material,
    isSource    :: Bool,
    power       :: Double,
    origin      :: Vec3 Double,
    radius      :: Double
} | Plane {
    material    :: Material,
    isSource    :: Bool,
    power       :: Double,
    normal      :: Vec3 Double,
    distance    :: Double
} deriving (Eq, Show)

intersection ray (Sphere {origin = o, radius = r})
    | disc > 0      = Just dist
    | otherwise     = Nothing    
    where   d       = direction ray
            p       = position ray
            a       = dot d d
            b       = 2 * dot d (p - o)
            c       = (dot (p - o) (p - o)) - (r * r)
            disc    = (b * b) - (4 * a * c)
            dist    = ((-b) - (sqrt disc)) / (2 * a)
            
intersection ray (Plane {normal = n, distance = d})
    | intersects    = Just e
    | otherwise     = Nothing
    where   intersects  = det /= 0 && e > 0
            det         = dot n (direction ray)
            e           = (d - dot n (position ray)) / det

findNormal :: Vec3 Double -> Primitive -> Vec3 Double
findNormal pos (Sphere {origin = o}) = normalize (pos - o)
findNormal pos (Plane {normal = n}) = n

newSphere m i o r = Sphere m i 1 o r
newPlane m n d = Plane m False 1 n d