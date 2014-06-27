module Main where

import Prelude hiding (readFile)

import Numerical
import Vec3 hiding (normalize)
import Scene
import Speaker
import Impulse

import Control.Applicative
import System.Environment
import Data.List (transpose)
import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as P
import System.IO hiding (readFile)
import Pipes.Aeson hiding (decoded, decode)
import Pipes.Aeson.Unchecked
import Control.Monad.State.Strict
import Lens.Family (view)
import Lens.Family.State.Strict 
import Data.Array.IO
import qualified Pipes.Prelude as PP
import Container (C3)

import qualified Sound.File.Sndfile as SF

import Data.Vector.Generic (fromList)
import Sound.File.Sndfile.Buffer.Vector (toBuffer)

spk :: [Speaker]
spk = [Speaker (Vec3 0 1 0) 0.5, Speaker (Vec3 1 0 0) 0.5]

lastSampleTimeForFile :: String -> IO Flt
lastSampleTimeForFile f = 
    withFile f ReadMode $ \hIn -> 
    evalStateT (zoom decoded (foldAll maxSample 0 id)) (P.fromHandle hIn)

maxSample :: Flt -> RayTrace -> Flt
maxSample = flip $ max . time . last . impulses

channelForFile :: FilePath -> Int -> Flt -> Speaker -> IO (IOArray Int (C3 Flt))
channelForFile f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    evalStateT (parseChannel t sr speaker) (P.fromHandle hIn)

produceRayTraces 
    :: Producer P.ByteString IO () 
    -> Producer RayTrace IO (Either (DecodingError, Producer P.ByteString IO ()) ())
produceRayTraces = view decoded 

produceRayTraces' :: Producer P.ByteString IO () -> Producer RayTrace IO ()
produceRayTraces' p = void $ produceRayTraces p 

whatever :: FilePath -> Int -> Flt -> Speaker -> IO (IOArray Int (C3 Flt))
whatever f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    PP.foldM (\ _ rt -> channelForRayTrace sr rt speaker t) 
             (return ()) 
             return 
             (produceRayTraces' $ P.fromHandle hIn)
    return t

whatever' :: FilePath -> Int -> Flt -> Speaker -> IO (IOArray Int (C3 Flt))
whatever' f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    runEffect $ produceRayTraces' (P.fromHandle hIn) 
                >-> PP.mapM (\ rt -> channelForRayTrace sr rt speaker t) 
                >-> PP.drain
    return t

parseChannel 
    :: IOArray Int (C3 Flt) 
    -> Flt 
    -> Speaker 
    -> StateT (Producer P.ByteString IO r) IO (IOArray Int (C3 Flt))
parseChannel t sr speaker = 
    zoom decoded $ foldAllM 
        (\ _ rt -> channelForRayTrace sr rt speaker t) 
        (return ()) 
        (\_ -> return t)

createAndProcessChannelForFile 
    :: String -> Int -> Flt -> Speaker -> IO (IOUArray Int Flt)
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
    let finalSample = secondsToSamples sr lastTime
    a <- mapM (createAndProcessChannelForFile f finalSample sr) speakers
    normalize a
    mapM getElems a

flatten :: String -> [Speaker] -> Flt -> String -> IO ()
flatten infile speakers sampleRate outFile = do
    channels <- createAllChannelsForFile infile sampleRate speakers
    let f = SF.Format SF.HeaderFormatAiff SF.SampleFormatFloat SF.EndianFile
    let info = SF.Info 
               (length (head channels)) 
               (round sampleRate) 
               (length channels) 
               f 
               1 
               True
    let sampleData = toBuffer $ fromList $ concat $ transpose channels
    _ <- SF.writeFile info outFile sampleData
    return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> flatten input spk 44100 output
        _ -> putStrLn "program takes two arguments"

