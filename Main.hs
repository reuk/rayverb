module Main where

import Impulse
import Primitive
import Mic
import Vec3
import Material
import Reflection
import Ray

import Data.WAVE
import Data.List

a   = newPlane (Material 0.95 0.95) (Vec3 1 0 0) (50)
b   = newPlane (Material 0.95 0.95) (Vec3 1 0 0) (-50)
c   = newPlane (Material 0.95 0.95) (Vec3 0 1 0) (10)
d   = newPlane (Material 0.95 0.95) (Vec3 0 1 0) (-10)
e   = newPlane (Material 0.95 0.95) (Vec3 0 0 1) (10)
f   = newPlane (Material 0.95 0.95) (Vec3 0 0 1) (-10)

s   = newSphere (Material 0.9 0.9) True (Vec3 40 5 5) 1

p   = [a, b, c, d, e, f, s]

source = getSources p

mica = newCardioid (Vec3 (-5) (-5) (-5)) (Vec3 1 0 0)
micb = newCardioid (Vec3 (-5) (-5) (-5)) (Vec3 0 1 0)

mics = [mica, micb]

--r   = traceMic p 1000 mic

r   = traceMics p 1000 mics

allSourceImpulses' s r = allSourceImpulses s (concat r)

i   = map (allSourceImpulses' source) r 

waveheader  = WAVEHeader (length mics) sampleRate 16 Nothing
--wavesamples = map (\x -> [doubleToSample x]) $ samples i
wavesamples = transpose $ map (map doubleToSample) $ multiSamples i

main = putWAVEFile "/Users/reuben/Desktop/trial3.wav" (WAVE waveheader wavesamples)
