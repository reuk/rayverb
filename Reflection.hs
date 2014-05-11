module Reflection where

import Numerical
import Material
import Control.DeepSeq

data Reflection = Reflection    {   surface :: Surface
                                ,   position :: Vec
                                ,   normal :: Vec
                                ,   distance :: Flt
                                ,   volume :: VolumeCollection
                                ,   direct :: Bool
                                }   deriving (Eq, Show)

instance NFData Reflection where
    rnf (Reflection a b c d e f) = a `deepseq`
                                   b `deepseq`
                                   c `deepseq`
                                   d `deepseq`
                                   e `deepseq`
                                   f `deepseq`
                                   ()
