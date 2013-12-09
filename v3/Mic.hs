module Mic where

import Vec3

data Mic = Mic {
    location    :: Vec3 Double
} deriving (Eq, Show)