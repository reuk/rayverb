module Reflection where

import Numerical
import Material

data Reflection = Reflection    {   surface :: Surface
                                ,   position :: Vec
                                ,   normal :: Vec
                                ,   distance :: Flt
                                ,   volume :: VolumeCollection
                                ,   direct :: Bool
                                }   deriving (Eq, Show)
