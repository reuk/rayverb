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

primitives =    [ constructPlane (C3 (Material 0.95 0.95)
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 1 0 0) (50)
                , constructPlane (C3 (Material 0.95 0.95) 
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 1 0 0) (-50)
                , constructPlane (C3 (Material 0.95 0.95) 
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 0 1 0) (10)
                , constructPlane (C3 (Material 0.95 0.95) 
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 0 1 0) (-10)
                , constructPlane (C3 (Material 0.95 0.95) 
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 0 0 1) (10)
                , constructPlane (C3 (Material 0.95 0.95) 
                                     (Material 0.85 0.85)
                                     (Material 0.75 0.75)) (Vec3 0 0 1) (-10)
                  
                , constructSphere (C3 (Material 1 1) 
                                      (Material 1 1)
                                      (Material 1 1)) True (Vec3 40 5 5) 1
                ]

mic = Mic $ Vec3 (-5) (-5) (-5)

spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

sampleRate :: Flt
sampleRate = 44100.0

rayverb primitives mic spk rays threshold sr filename = do
    r <- traceMic primitives mic rays threshold
    channels <- createAllChannels (lastSample sr r) sampleRate r spk
    putWAVEFile filename (WAVE waveheader 
        (map (map doubleToSample) (transpose channels)))
    where   waveheader = WAVEHeader (length spk) (round sampleRate) 16 Nothing
    
main :: IO ()
main = rayverb primitives mic spk 1000 0.01 44100 "/Users/reuben/Desktop/out.wav"
