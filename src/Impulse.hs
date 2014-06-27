{-# LANGUAGE DeriveGeneric #-}

module Impulse where

import Numerical
import Vec3
import Material
import Container
import qualified Primitive as P
import qualified Reflection as R
import Data.Aeson
import GHC.Generics

import Control.Applicative

import Control.DeepSeq

data Impulse = Impulse  {   time :: Flt
                        ,   amplitude :: C3 Flt
                        }   deriving (Eq, Show, Generic)

instance FromJSON Impulse
instance ToJSON Impulse

instance NFData Impulse where
    rnf (Impulse t a) = t `deepseq` a `deepseq` ()

speedOfSound :: Flt
speedOfSound = 340

secondsPerMetre :: Flt
secondsPerMetre = 1 / speedOfSound

constructImpulse :: P.Primitive -> R.Reflection -> Impulse
constructImpulse source r =
    Impulse 
        (force (secondsPerMetre * (R.distance r + magnitude diff - P.radius source)))
        (force (   if R.direct r 
                   then R.volume r
                   else pure (dot (R.normal r) (normalize diff)) * R.volume r * fmap diffuse (R.surface r)
        ))
    where   diff = P.origin source - R.position r

secondsToSamples :: Integral b => Flt -> Flt -> b
secondsToSamples sampleRate t = round (sampleRate * t)

timeInSamples :: Integral b => Flt -> Impulse -> b
timeInSamples sampleRate = secondsToSamples sampleRate . time
