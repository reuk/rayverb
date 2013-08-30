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
}

intersects :: Ray -> Primitive -> Bool
intersects ray (Sphere {material = m, isSource = i, origin = o, radius = r}) =
    determinant > 0
    where   determinant = (b * b) - (dot v v) + (r * r)
            b           = - dot v (direction ray)
            v           = (position ray) - o

intersects ray p@(Plane {material = m, isSource = i, normal = n, distance = d}) =
    d != 0 && e > 0
    where   d = dot n (direction ray)
            e = distanceToIntersection ray p 

distanceToIntersection :: Ray -> Primitive -> Double
    
