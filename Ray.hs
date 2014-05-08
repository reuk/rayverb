module Ray where

import Numerical
import Vec3
import Positionable
import Directionable

import Data.Maybe

data Ray = Ray Vec Vec
    deriving (Eq, Show)

instance Positionable Ray where
    position (Ray x _) = x

instance Directionable Ray where
    direction (Ray _ x) = x

reflectRay ray normal intersection = Ray intersection 
                                   $ reflect normal 
                                   $ direction ray
