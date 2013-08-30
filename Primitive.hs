module Primitive where

import Vec3
import Material

data Primitive = Sphere {
    material    :: Material,
    isSource    :: Bool,
    origin      :: Vec3 Double,
    radius      :: Double
} | Plane {
    material    :: Material,
    isSource    :: Bool,
    direction   :: Vec3 Double,
    distance    :: Double
}
