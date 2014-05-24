module Main where

import Prelude hiding (readFile)

import Numerical
import Vec3 hiding (normalize)
import Scene
import Speaker
import Impulse

import Data.WAVE
import Control.Applicative
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
import Lens.Family
import Lens.Family.State.Strict 
import Data.Array.IO
import Debug.Trace
import qualified Pipes.Prelude as PP
import Control.Foldl (purely)
import Lens.Family (view)

spk :: [Speaker]
spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

lastSampleTimeForFile :: String -> IO Flt
lastSampleTimeForFile f = 
    withFile f ReadMode $ \hIn -> 
    evalStateT (zoom decoded (foldAll maxSample 0 id)) (P.fromHandle hIn)

maxSample = flip $ max . time . last . impulses

channelForFile f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    evalStateT (parseChannel t sr speaker) (P.fromHandle hIn)

produceRayTraces :: Producer P.ByteString IO () -> Producer RayTrace IO (Either (DecodingError, Producer P.ByteString IO ()) ())
produceRayTraces = view decoded 

produceRayTraces' :: Producer P.ByteString IO () -> Producer RayTrace IO ()
produceRayTraces' p = produceRayTraces p >> return ()

whatever f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    PP.foldM (\ _ rt -> channelForRayTrace sr rt speaker t) (return ()) return (produceRayTraces' $ P.fromHandle hIn)
    return t

whatever' f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    runEffect $ (produceRayTraces' $ P.fromHandle hIn) >-> (PP.mapM (\ rt -> channelForRayTrace sr rt speaker t)) >-> PP.drain
    return t

parseChannel t sr speaker = 
    zoom decoded $ foldAllM 
        (\ _ rt -> channelForRayTrace sr rt speaker t) 
        (return ()) 
        (\_ -> return t)

createAndProcessChannelForFile :: String -> Int -> Flt -> Speaker -> IO (IOUArray Int Flt)
createAndProcessChannelForFile f samples sr speaker = do
    channel <- whatever' f samples sr speaker
    bands <- splitBands channel
    filterBands sr bands
    out <- compileBands bands
    hipass sr 20 out
    return out

createAllChannelsForFile :: String -> Flt -> [Speaker] -> IO [[Flt]]
createAllChannelsForFile f sr speakers = do
    lastTime <- lastSampleTimeForFile f
    let lastSample = secondsToSamples sr lastTime
    a <- mapM (createAndProcessChannelForFile f lastSample sr) speakers
    normalize a
    mapM getElems a

flatten :: String -> [Speaker] -> Flt -> Int -> String -> IO ()
flatten inFile speakers sampleRate bitDepth outFile = do
    channels <- createAllChannelsForFile inFile sampleRate speakers
    let out = force $ (map (map doubleToSample) $!! (transpose channels))
    putWAVEFile outFile (WAVE waveheader out)
    where   waveheader = WAVEHeader (length speakers) (round sampleRate) bitDepth Nothing

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> 
            flatten (args !! 0) spk 44100 16 (args !! 1)
--         2 -> do
--             last <- lastSampleTimeForFile (args !! 0)
--             print $ show last
        _ -> putStrLn "program takes two arguments"

