module Main where

import Impulse
import Primitive
import Mic
import Vec3
import Material
import Reflection
import Ray

import Data.WAVE

a   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (50)
b   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (-50)
c   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (10)
d   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (-10)
e   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (10)
f   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (-10)

s   = newSphere (Material 0.9 0.9) True (Vec3 40 5 5) 1

p   = [a, b, c, d, e, f, s]

source = getSources p

mic = newCardioid (Vec3 (-5) (-5) (-5)) (Vec3 1 2 3)

r   = traceMic p 1000 mic

i   = allSourceImpulses source (concat r)  

waveheader  = WAVEHeader 1 sampleRate 16 Nothing
wavesamples = map (\x -> [doubleToSample x]) $ samples i

main = putWAVEFile "/Users/reuben/Desktop/trial3.wav" (WAVE waveheader wavesamples)
