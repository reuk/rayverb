module Main where

import Numerical
import Vec3
import Scene
import Primitive
import Microphone
import Speaker
import Material
import Container

import Data.WAVE
import Data.List

a   = constructPlane (C3 (Material 0.95 0.95)
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 1 0 0) (50)
b   = constructPlane (C3 (Material 0.95 0.95) 
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 1 0 0) (-50)
c   = constructPlane (C3 (Material 0.95 0.95) 
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 0 1 0) (10)
d   = constructPlane (C3 (Material 0.95 0.95) 
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 0 1 0) (-10)
e   = constructPlane (C3 (Material 0.95 0.95) 
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 0 0 1) (10)
f   = constructPlane (C3 (Material 0.95 0.95) 
                         (Material 0.85 0.85)
                         (Material 0.75 0.75)) (Vec3 0 0 1) (-10)

s   = constructSphere (C3 (Material 1 1) 
                          (Material 1 1)
                          (Material 1 1)) True (Vec3 40 5 5) 1

p   = [a, b, c, d, e, f, s]

mic = Mic $ Vec3 (-5) (-5) (-5)

spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 0 0 1) 0.5]

sampleRate :: Flt
sampleRate = 44100.0

main :: IO ()
main = do
    r <- traceMic p mic 100000 0.01
    channels <- createAllChannels (lastSample sampleRate r) sampleRate r spk
    putWAVEFile "/Users/reuben/Desktop/trial.wav" (WAVE waveheader 
        (transpose $ map (map doubleToSample) channels))
    where   waveheader = WAVEHeader 1 (round sampleRate) 16 Nothing
