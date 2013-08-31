module Mic where

import Vec3

data Vardioid = Vardioid {
    location    :: Vec3 Double,
    pointing    :: Vec3 Double,
    coeff       :: Double
} deriving (Eq, Show)

newOmni :: Vec3 Double -> Vardioid
newOmni loc     = Vardioid {location = loc, pointing = (Vec3 1 0 0), coeff = 0}

newCardioid :: Vec3 Double -> Vec3 Double -> Vardioid
newCardioid loc point   = Vardioid {location = loc, pointing = point, coeff = 0.5}

newBipolar :: Vec3 Double -> Vec3 Double -> Vardioid
newBipolar loc point    = Vardioid {location = loc, pointing = point, coeff = 1}

getAttenuation :: Vardioid -> Vec3 Double -> Double
getAttenuation mic dir = (1 - c) + (c * (cos angle)) 
    where   c       = coeff mic
            angle   = acos $ dot rel (pointing mic)
            rel     = normalize dir
