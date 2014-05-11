module Main where

import Numerical
import Vec3
import Scene
import Speaker

import Data.WAVE

import System.Environment
import Data.List (transpose)
import Control.DeepSeq
import Text.JSON
import Text.JSON.Generic

spk :: [Speaker]
spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

sampleRate :: Flt
sampleRate = 44100.0

flattener :: [Speaker] -> String -> String -> IO ()
flattener speakers raytraceFile outFile = do
    input <- readFile raytraceFile
    let x = decode input :: Result JSValue
    case x of
        Ok y -> case fromJSON y of
            Ok z -> do
                channels <- createAllChannels (lastSample sampleRate z) sampleRate z speakers
                let out = force $ (map (map doubleToSample) $!! (transpose channels))
                putWAVEFile outFile (WAVE waveheader out)
                where   waveheader = WAVEHeader (length speakers) (round sampleRate) 16 Nothing
            Error y -> putStrLn y
        Error y -> putStrLn y
    
main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> flattener spk (args !! 0) (args !! 1)
        _ -> putStrLn "program takes two arguments"
