module Ray where

import Vec3

data Ray = Ray {
    origin      :: Vec3,
    direction   :: Vec3
}
