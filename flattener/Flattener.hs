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
import Data.List (transpose, foldl1')
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
import Data.Array.IO

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
    where   maximum = foldl1' max

-- remember always the beauty of this function
-- actually don't because it's not strict in the argument to 'max' so
-- it gradually builds up thunks linearly with the number of raytraces
lastSampleTimeForFile :: String -> IO Flt
lastSampleTimeForFile f = withFile f ReadMode $ \hIn -> do
    evalStateT parseLastSampleTime (P.fromHandle hIn)

parseLastSampleTime = zoom decoded (foldAll (flip $ max . rayTraceLength) 0 id)

channelForFile f l sr speaker = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    evalStateT (parseChannel t sr speaker) (P.fromHandle hIn)
    return t

parseChannel t sr speaker = 
    zoom decoded $ foldAllM 
        (\ _ rt -> channelForRayTrace sr rt speaker t) 
        (return ()) 
        return

createAndProcessChannelForFile :: String -> Int -> Flt -> Speaker -> IO (IOUArray Int Flt)
createAndProcessChannelForFile f samples sr speaker = do
    channel <- channelForFile f samples sr speaker
    evalArray channel
    bands <- splitBands channel
    filterBands sr bands
    out <- compileBands bands
    evalArray out
    hipass sr 20 out
    evalArray out
    return out

createAllChannelsForFile :: String -> Flt -> [Speaker] -> IO [[Flt]]
createAllChannelsForFile f sr speakers = do
    lastTime <- lastSampleTimeForFile f
    let lastSample = secondsToSamples sampleRate lastTime
    a <- mapM (createAndProcessChannelForFile f lastSample sr) speakers
    normalize a
    mapM getElems a

main = do
    args <- getArgs
    case length args of
        2 -> do
            channels <- createAllChannelsForFile (args !! 0) sampleRate spk
            let out = force $ (map (map doubleToSample) $!! (transpose channels))
            putWAVEFile (args !! 1) (WAVE waveheader out)
            where   waveheader = WAVEHeader (length spk) (round sampleRate) 16 Nothing
        _ -> putStrLn "program takes two arguments"

