module Impulse where

import Numerical
import Vec3
import Material
import qualified Primitive as P
import qualified Reflection as R

import Control.Applicative

data Impulse = Impulse  {   time :: Flt
                        ,   amplitude :: VolumeCollection
                        }   deriving (Eq, Show)

speedOfSound :: Flt
speedOfSound = 340

secondsPerMetre :: Flt
secondsPerMetre = 1 / speedOfSound

constructImpulse :: P.Primitive -> R.Reflection -> Impulse
constructImpulse source r =
    Impulse 
        (secondsPerMetre * (R.distance r + magnitude diff - P.radius source))
        (   if R.direct r 
            then R.volume r
            else pure (dot (R.normal r) (normalize diff)) * R.volume r * fmap diffuse (R.surface r)
        )
    where   diff = P.origin source - R.position r

timeInSamples :: Integral b => Flt -> Impulse -> b
timeInSamples sampleRate impulse = round (sampleRate * time impulse)
