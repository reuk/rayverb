module Main where

import Prelude hiding (all, any, concat, maximum, foldr)

import Numerical
import Vec3
import Scene
import Primitive
import Microphone
import Speaker
import Material
import Container

import Data.WAVE

import System.Environment
import Data.List (transpose)
import Control.DeepSeq

primitives :: [Primitive]
primitives =    [ Plane (C3 (Material 0.95 0.95)
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 1 0 0) 50
                , Plane (C3 (Material 0.95 0.95) 
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 1 0 0) (-50)
                , Plane (C3 (Material 0.95 0.95) 
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 0 1 0) 10
                , Plane (C3 (Material 0.95 0.95) 
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 0 1 0) (-10)
                , Plane (C3 (Material 0.95 0.95) 
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 0 0 1) 10
                , Plane (C3 (Material 0.95 0.95) 
                            (Material 0.85 0.85)
                            (Material 0.75 0.75)) (Vec3 0 0 1) (-10)
                  
                , Sphere (C3 (Material 1 1) 
                             (Material 1 1)
                             (Material 1 1)) True (Vec3 40 5 5) 1
                ]

mic :: Microphone
mic = Mic $ Vec3 (-5) (-5) (-5)

spk :: [Speaker]
spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

sampleRate :: Flt
sampleRate = 44100.0

rayverb :: [Primitive] -> Microphone -> [Speaker] -> Int -> Flt -> Flt -> String -> IO ()
rayverb prims m s rays threshold sr filename = do
    r <- traceMic prims m rays threshold
    channels <- createAllChannels (lastSample sr r) sr r s
    let out = force $ (map (map doubleToSample) $!! (transpose channels))
    putWAVEFile filename (WAVE waveheader out)
    where   waveheader = WAVEHeader (length s) (round sampleRate) 16 Nothing
    
main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> rayverb primitives mic spk 10000 0.01 44100 $ head args
        _ -> putStrLn "program takes one argument"

