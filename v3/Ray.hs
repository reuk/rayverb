module Ray where

import Vec3

data Ray = Ray {
    position    :: Vec3 Double,
    direction   :: Vec3 Double
} deriving (Eq, Show)

reflection nor int ray = Ray int dir
    where   dir = reflect nor (direction ray)
