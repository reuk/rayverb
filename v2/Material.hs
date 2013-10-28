module Material where

data Material = Material {
    diffuse     :: Double,
    reflective  :: Double
} deriving (Eq, Show)