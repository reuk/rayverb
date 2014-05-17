module Main where

import Prelude hiding (readFile)

import Numerical
import Vec3
import Scene
import Speaker
import Impulse

import Data.WAVE

import System.Environment
import Data.List (transpose)
import Control.DeepSeq
import Data.Aeson hiding (decode)
import Data.ByteString.Lazy (readFile)

import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as P
import System.IO hiding (readFile)
import Pipes.Aeson hiding (decoded, decode)
import Pipes.Aeson.Unchecked

import Control.Monad.State.Strict

import Lens.Family.State.Strict

spk :: [Speaker]
spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

sampleRate :: Flt
sampleRate = 44100.0

flattener :: [Speaker] -> String -> String -> IO ()
flattener speakers raytraceFile outFile = do
    input <- readFile raytraceFile
    let x = decode' input :: Maybe [RayTrace]
    case x of
        Just z -> do
            channels <- createAllChannels (lastSample sampleRate z) sampleRate z speakers
            let out = force $ (map (map doubleToSample) $!! (transpose channels))
            putWAVEFile outFile (WAVE waveheader out)
            where   waveheader = WAVEHeader (length speakers) (round sampleRate) 16 Nothing
        Nothing -> putStrLn "parse failed"
    
main :: IO ()
-- main = do
--     args <- getArgs
--     case length args of
--         2 -> flattener spk (args !! 0) (args !! 1)
--         _ -> putStrLn "program takes two arguments"

rayTraceLength (RayTrace _ impulses) = maximum $ map time impulses

-- remember always the beauty of this function
lastSampleTimeForFile :: String -> IO Flt
lastSampleTimeForFile f = withFile f ReadMode $ \hIn -> do
    evalStateT 
        (zoom decoded (foldAll (flip $ max . rayTraceLength) 0 id))
        (P.fromHandle hIn)

main = do
    args <- getArgs
    case length args of
        2 -> do
            last <- lastSampleTimeForFile $ head args
            print $ show last
        _ -> putStrLn "program takes two arguments"

