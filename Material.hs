module Material where

import Numerical
import Container

data Material = Material    {   diffuse :: Flt
                            ,   specular :: Flt
                            }   deriving (Eq, Show)

type Surface = C3 Material
type VolumeCollection = C3 Flt
