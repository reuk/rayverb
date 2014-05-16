module Ray where

import Numerical
import Vec3
import Positionable
import Directionable
import Control.DeepSeq

data Ray = Ray Vec Vec
    deriving (Eq, Show)

instance Positionable Ray where
    position (Ray x _) = x

instance Directionable Ray where
    direction (Ray _ x) = x

instance NFData Ray where
    rnf (Ray a b) = a `deepseq` b `deepseq` ()

reflectRay :: Directionable c => c -> Vec3 Flt -> Vec -> Ray
reflectRay ray normal intersection = Ray intersection 
                                   $ reflect normal 
                                   $ direction ray
