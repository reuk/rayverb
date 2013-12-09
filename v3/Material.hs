module Material where

import Container

data Material = Material {
    diffuse     :: Double,
    reflective  :: Double
} deriving (Eq, Show)

type Surface = C3 Material