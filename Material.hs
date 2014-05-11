module Material where

import Numerical
import Container
import Control.DeepSeq

data Material = Material    {   diffuse :: Flt
                            ,   specular :: Flt
                            }   deriving (Eq, Show)

instance NFData Material where
    rnf (Material a b) = a `deepseq`
                         b `deepseq`
                         ()

type Surface = C3 Material
type VolumeCollection = C3 Flt
