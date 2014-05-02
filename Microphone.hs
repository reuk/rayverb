module Microphone where

import Numerical
import Vec3
import Positionable
import Ray

import System.Random

data Microphone = Mic Vec

instance Positionable Microphone where
    position (Mic x) = x

constructRaysFromMic :: Microphone -> IO [Ray]
constructRaysFromMic (Mic position) = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    return $ map (constructRay position) $ zipWith unitVectorFromAngles 
                                            (randomRs ((-1), 1) gen1)
                                            (randomRs (-pi, pi) gen2)

unitVectorFromAngles :: Flt -> Flt -> Vec
unitVectorFromAngles r phi = Vec3 (z2 * (cos phi)) (z2 * (sin phi)) r
    where   z2 = sqrt (1.0 - (r * r))
