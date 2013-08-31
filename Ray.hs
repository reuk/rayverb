module Ray where

import Vec3

data Ray = Ray {
    position    :: Vec3 Double,
    direction   :: Vec3 Double,
    volume      :: Double
} deriving (Eq, Show)

reflection nor int ray = Ray pos dir
    where   pos = int
            dir = reflect nor (direction ray)
