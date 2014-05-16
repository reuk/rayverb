{-# LANGUAGE DeriveGeneric #-}

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

import Control.DeepSeq

import Data.Aeson
import GHC.Generics

raytrace :: [P.Primitive] -> [P.Primitive] -> Flt -> Ray -> Flt -> C3 Flt -> [Impulse]
raytrace primitives sources threshold ray distance volume
    | all (< threshold) volume = []
    | isNothing maybeClosest = []
    | primitiveIsSource prim = impulses
    | otherwise = impulses ++ (force $ raytrace primitives sources threshold newRay newDist newVol)
    where   maybeClosest = P.closest ray primitives
            prim = fromJust maybeClosest
            newRay = P.reflectFromPrimitive prim ray
            intersection = position newRay
            newDist = distance + magnitude (position ray - intersection)
            newVol = abop ((*) . specular) (P.surface prim) volume
            impulses = force $ toImpulsesAllSources sources reflection
            reflection = force $ R.Reflection   (P.surface prim) 
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
traceToImpulse primitives ray threshold = raytrace primitives (getSources primitives) threshold ray 0 (pure 1)

data RayTrace = RayTrace Vec [Impulse]
    deriving (Eq, Show, Generic)

instance FromJSON RayTrace
instance ToJSON RayTrace

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

channelUpdateLoop :: Flt -> Flt -> [Impulse] -> IOArray Int (C3 Flt) -> IO ()
channelUpdateLoop sr coeff impulses arr = 
    mapM_ (\ x -> writeArray arr (timeInSamples sr x) (pure coeff * amplitude x)) impulses

channelForRayTrace :: Flt -> RayTrace -> Speaker -> IOArray Int (C3 Flt) -> IO ()
channelForRayTrace sampleRate (RayTrace dir impulses) speaker = 
    channelUpdateLoop sampleRate (attenuation speaker dir) impulses 

channelForAllRayTraces :: Flt -> [RayTrace] -> Speaker -> IOArray Int (C3 Flt) -> IO ()
channelForAllRayTraces sampleRate rt speaker arr = 
    mapM_ (\ x -> channelForRayTrace sampleRate x speaker arr) rt

createChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO (IOArray Int (C3 Flt))
createChannel samples sampleRate raytraces speaker = do
    t <- newArray (0, samples) (pure 0)
    channelForAllRayTraces sampleRate raytraces speaker t
    return t

evalArray :: (Num i, Ix i, NFData e, MArray a e m) => a i e -> m ()
evalArray arr = do
    (mini, maxi) <- getBounds arr
    worker mini maxi
        where   worker ind maxi
                    | ind > maxi = return ()
                    | otherwise = do
                        val <- readArray arr ind
                        deepseq val $ worker (ind + 1) maxi

boxedToUnboxed :: ((C3 Flt) -> Flt) -> IOArray Int (C3 Flt) -> IO (IOUArray Int Flt)
boxedToUnboxed func arr = do
    elems <- getElems arr
    bounds <- getBounds arr
    newListArray bounds $ map func elems

splitBands :: IOArray Int (C3 Flt) -> IO (C3 (IOUArray Int Flt))
splitBands vc = do
    ub0 <- boxedToUnboxed c30 vc
    ub1 <- boxedToUnboxed c31 vc
    ub2 <- boxedToUnboxed c32 vc
    return $! C3 ub0 ub1 ub2

filterBands :: Flt -> C3 (IOUArray Int Flt) -> IO ()
filterBands sampleRate (C3 b0 b1 b2) = do
    lopass sampleRate 200 b0
    evalArray b0
    bandpass sampleRate 200 2000 b1
    evalArray b1
    hipass sampleRate 2000 b2
    evalArray b2

compileBands :: C3 (IOUArray Int Flt) -> IO (IOUArray Int Flt)
compileBands (C3 b0 b1 b2) = do
    (mini, maxi) <- getBounds b0
    out <- newArray (mini, maxi) 0.0
    worker out mini maxi
    evalArray out
    return out
        where   worker out ind maxi
                    | ind > maxi = return ()
                    | otherwise = do
                        v0 <- readArray b0 ind
                        v1 <- readArray b1 ind
                        v2 <- readArray b2 ind
                        writeArray out ind (v0 + v1 + v2)
                        worker out (ind + 1) maxi

lopass :: Flt -> Flt -> IOUArray Int Flt -> IO ()
lopass sampleRate cutoff band = do
    (mini, maxi) <- getBounds band
    lopassWorker (1 - exp (-2 * pi * cutoff / sampleRate)) mini maxi 0 
    evalArray band
    where   lopassWorker a0 ind maxIndex state 
                | ind > maxIndex = return ()
                | otherwise = do
                    val <- readArray band ind
                    let new = state + a0 * (val - state)
                    writeArray band ind new
                    lopassWorker a0 (ind + 1) maxIndex new 

hipass :: Flt -> Flt -> IOUArray Int Flt -> IO ()
hipass sampleRate cutoff band = do
    (mini, maxi) <- getBounds band
    hipassWorker (1 - exp (-2 * pi * cutoff / sampleRate)) mini maxi 0 
    evalArray band
    where   hipassWorker a0 ind maxIndex state 
                | ind > maxIndex = return()
                | otherwise = do
                    val <- readArray band ind
                    let new = state + a0 * (val - state)
                    writeArray band ind (val - new)
                    hipassWorker a0 (ind + 1) maxIndex new 

bandpass :: Flt -> Flt -> Flt -> IOUArray Int Flt -> IO ()
bandpass sampleRate lo hi band = do
    lopass sampleRate hi band
    evalArray band
    hipass sampleRate lo band
    evalArray band

normalize :: [IOUArray Int Flt] -> IO ()
normalize channels = do
    maxes <- mapM maxAbs channels
    let maxi = maximum maxes
    mapM_ (setGain (0.99 / maxi)) channels

maxAbs :: IOUArray Int Flt -> IO Flt
maxAbs arr = do
    (mini, maxi) <- getBounds arr
    worker 0 mini maxi
    where   worker state ind maxi 
                | ind > maxi = return state
                | otherwise = do
                    val <- readArray arr ind
                    let new = if abs val > state then abs val else state
                    worker new (ind + 1) maxi

setGain :: Flt -> IOUArray Int Flt -> IO ()
setGain gain arr = do
    (mini, maxi) <- getBounds arr
    worker mini maxi
    evalArray arr
        where   worker ind maxi
                    | ind > maxi = return ()
                    | otherwise = do
                        val <- readArray arr ind
                        writeArray arr ind (val * gain)
                        worker (ind + 1) maxi

createAndProcessChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO (IOUArray Int Flt)
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
