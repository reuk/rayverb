module Scene where

import Prelude hiding (all, any, concat, concatMap, maximum, foldr, mapM_)

import Numerical
import Vec3 hiding (normalize)
import Positionable
import Directionable
import Ray
import qualified Primitive as P
import Material
import qualified Reflection as R
import Impulse
import Microphone
import Speaker
import Container
import ApplicativeBinaryOp

import Data.Maybe
import Data.Foldable
import Data.Array.IO
import Control.Applicative
import Debug.Trace
import Control.DeepSeq

import Control.DeepSeq

raytrace :: [P.Primitive] -> Ray -> Flt -> VolumeCollection -> [R.Reflection]
raytrace primitives ray distance volume 
    | isNothing maybeClosest = []
    | primitiveIsSource prim = [reflect]
    | otherwise = reflect : raytrace primitives newRay newDist newVol
    where   maybeClosest = P.closest ray primitives
            prim = fromJust maybeClosest
            newRay = P.reflectFromPrimitive prim ray
            intersection = position newRay
            newDist = distance + magnitude (position ray - intersection)
            newVol = abop ((*) . specular) (P.surface prim) volume
            reflect = R.Reflection  (P.surface prim) 
                                    intersection
                                    (P.findNormal prim intersection)
                                    newDist
                                    newVol
                                    (primitiveIsSource prim)

primitiveIsSource :: P.Primitive -> Bool
primitiveIsSource (P.Sphere _ isSource _ _) = isSource
primitiveIsSource _ = False

getSources :: [P.Primitive] -> [P.Primitive]
getSources = filter primitiveIsSource

toImpulsesAllSources :: [P.Primitive] -> R.Reflection -> [Impulse]
toImpulsesAllSources primitives reflection = 
    map (`constructImpulse` reflection) primitives

traceToImpulse :: [P.Primitive] -> Ray -> Flt -> [Impulse]
traceToImpulse primitives ray threshold = 
    concatMap (toImpulsesAllSources (getSources primitives)) reflections
    where   reflections = takeWhile (any (> threshold) . R.volume)
                                    (raytrace primitives ray 0 (pure 1))

data RayTrace = RayTrace Vec [Impulse]
    deriving (Eq, Show)

traceDirection :: [P.Primitive] -> Ray -> Flt -> RayTrace
traceDirection prims ray = RayTrace (direction ray) . traceToImpulse prims ray

traceMic :: [P.Primitive] -> Microphone -> Int -> Flt -> IO [RayTrace]
traceMic prims mic rayNum threshold = do
    rays <- constructRaysFromMic mic
    return $ map (\ x -> traceDirection prims x threshold) $ take rayNum rays

lastSample :: Flt -> [RayTrace] -> Int
lastSample sampleRate = maximum . map (\ (RayTrace _ i) -> maximum (map (timeInSamples sampleRate) i))

trimRayTraces :: Int -> Flt -> [RayTrace] -> [RayTrace]
trimRayTraces samples sampleRate =
    map (\ (RayTrace d i) -> RayTrace d $ filter ((>) samples . timeInSamples sampleRate) i) 

channelUpdateLoop :: Flt -> Flt -> [Impulse] -> IOArray Int VolumeCollection -> IO ()
channelUpdateLoop sr coeff impulses arr = 
    mapM_ (\ x -> writeArray arr (timeInSamples sr x) (pure coeff * amplitude x)) impulses

channelForRayTrace :: Flt -> RayTrace -> Speaker -> IOArray Int VolumeCollection -> IO ()
channelForRayTrace sampleRate (RayTrace dir impulses) speaker = 
    channelUpdateLoop sampleRate (attenuation speaker dir) impulses 

channelForAllRayTraces :: Flt -> [RayTrace] -> Speaker -> IOArray Int VolumeCollection -> IO ()
channelForAllRayTraces sampleRate raytrace speaker arr = 
    mapM_ (\ x -> channelForRayTrace sampleRate x speaker arr) raytrace

createChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO (IOArray Int VolumeCollection)
createChannel samples sampleRate raytraces speaker = do
    t <- newArray (0, samples) (pure 0)
    channelForAllRayTraces sampleRate raytraces speaker t
    return t

evalArray arr = do
    (min, max) <- getBounds arr
    worker min max
        where   worker index max
                    | index > max = return ()
                    | otherwise = do
                        val <- readArray arr index
                        deepseq val $ worker (index + 1) max

splitBands :: IOArray Int (C3 Flt) -> IO (C3 (IOArray Int Flt))
splitBands vc = do
    b0 <- mapArray c3_0 vc
    evalArray b0
    b1 <- mapArray c3_1 vc
    evalArray b1
    b2 <- mapArray c3_2 vc
    evalArray b2
    return $ C3 b0 b1 b2

filterBands :: Flt -> C3 (IOArray Int Flt) -> IO ()
filterBands sampleRate (C3 b0 b1 b2) = do
    lopass sampleRate 200 b0
    evalArray b0
    bandpass sampleRate 200 2000 b1
    evalArray b1
    hipass sampleRate 2000 b2
    evalArray b2

compileBands :: C3 (IOArray Int Flt) -> IO (IOArray Int Flt)
compileBands (C3 b0 b1 b2) = do
    (min, max) <- getBounds b0
    out <- newArray (min, max) 0.0
    worker out min max
    return out
        where   worker out index max
                    | index > max = return ()
                    | otherwise = do
                        v0 <- readArray b0 index
                        v1 <- readArray b1 index
                        v2 <- readArray b2 index
                        writeArray out index (v0 + v1 + v2)
                        worker out (index + 1) max

lopass :: Flt -> Flt -> IOArray Int Flt -> IO ()
lopass sampleRate cutoff band = do
    (min, max) <- getBounds band
    lopassWorker (1 - exp (-2 * pi * cutoff / sampleRate)) min max 0 band
    evalArray band
    where   lopassWorker a0 index maxIndex state band
                | index > maxIndex = return ()
                | otherwise = do
                    val <- readArray band index
                    let new = state + a0 * (val - state)
                    writeArray band index new
                    lopassWorker a0 (index + 1) maxIndex new band

hipass :: Flt -> Flt -> IOArray Int Flt -> IO ()
hipass sampleRate cutoff band = do
    (min, max) <- getBounds band
    hipassWorker (1 - exp (-2 * pi * cutoff / sampleRate)) min max 0 band
    evalArray band
    where   hipassWorker a0 index maxIndex state band
                | index > maxIndex = return()
                | otherwise = do
                    val <- readArray band index
                    let new = state + a0 * (val - state)
                    writeArray band index (val - new)
                    hipassWorker a0 (index + 1) maxIndex new band

bandpass sampleRate lo hi band = do
    lopass sampleRate hi band
    evalArray band
    hipass sampleRate lo band
    evalArray band

normalize :: [IOArray Int Flt] -> IO ()
normalize channels = do
    maxes <- mapM maxAbs channels
    let max = maximum maxes
    mapM_ (setGain (0.99 / max)) channels

maxAbs :: IOArray Int Flt -> IO Flt
maxAbs arr = do
    (min, max) <- getBounds arr
    worker 0 min max
    where   worker state index max 
                | index > max = return state
                | otherwise = do
                    val <- readArray arr index
                    let new = if abs val > state then abs val else state
                    worker new (index + 1) max

setGain :: Flt -> IOArray Int Flt -> IO ()
setGain gain arr = do
    (min, max) <- getBounds arr
    worker gain min max
    evalArray arr
        where   worker gain index max
                    | index > max = return ()
                    | otherwise = do
                        val <- readArray arr index
                        writeArray arr index (val * gain)
                        worker gain (index + 1) max

createAndProcessChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO (IOArray Int Flt)
createAndProcessChannel samples sr raytraces speaker = do
    channel <- createChannel samples sr raytraces speaker
    evalArray channel
    bands <- splitBands channel
    filterBands sr bands
    out <- compileBands bands
    evalArray out
    hipass sr 20 out
    evalArray out
    return out

createAllChannels :: Int -> Flt -> [RayTrace] -> [Speaker] -> IO [[Flt]]
createAllChannels samples sampleRate raytraces speakers = do
    a <- mapM (createAndProcessChannel samples sampleRate raytraces) speakers
    normalize a
    mapM getElems a
