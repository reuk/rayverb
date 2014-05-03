module Scene where

import Prelude hiding (all, concat, maximum, foldr)

import Numerical
import Vec3
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

raytrace :: [P.Primitive] -> Ray -> Flt -> VolumeCollection -> [R.Reflection]
raytrace primitives ray distance volume 
    | isNothing maybeClosest = []
    | primitiveIsSource prim = [reflect]
    | otherwise = reflect : raytrace primitives newRay newDist newVol 
    where   maybeClosest = P.closest ray primitives
            prim = fromJust maybeClosest
            newRay = P.reflectFromPrimitive prim ray
            intersection = position newRay
            newDist = distance + magnitude ((position ray) - intersection)
            newVol = abop ((*) . specular) (P.surface prim) volume
            reflect = R.Reflection  (P.surface prim) 
                                    intersection
                                    (P.findNormal prim intersection)
                                    newDist
                                    newVol
                                    (primitiveIsSource prim)

primitiveIsSource (P.Sphere _ isSource _ _) = isSource
primitiveIsSource _ = False

getSources :: [P.Primitive] -> [P.Primitive]
getSources = filter primitiveIsSource

toImpulsesAllSources :: [P.Primitive] -> R.Reflection -> [Impulse]
toImpulsesAllSources primitives reflection = 
    map (\ x -> constructImpulse x reflection) primitives

traceToImpulse :: [P.Primitive] -> Ray -> Flt -> [Impulse]
traceToImpulse primitives ray threshold = 
    concat $ map (toImpulsesAllSources (getSources primitives)) reflections
    where   reflections = takeWhile (\ x -> all (> threshold) (R.volume x)) 
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

channelUpdateLoop :: Flt -> Flt -> [Impulse] -> IOArray Int VolumeCollection -> IO (IOArray Int VolumeCollection)
channelUpdateLoop _ _ [] arr = return arr
channelUpdateLoop sampleRate coeff (x:xs) arr = do
    writeArray arr (timeInSamples sampleRate x) ((pure coeff) * (amplitude x))
    channelUpdateLoop sampleRate coeff xs arr

channelForRayTrace :: Flt -> RayTrace -> Speaker -> IOArray Int VolumeCollection -> IO (IOArray Int VolumeCollection)
channelForRayTrace sampleRate (RayTrace dir impulses) speaker arr = 
    channelUpdateLoop sampleRate (attenuation speaker dir) impulses arr

channelForAllRayTraces :: Flt -> [RayTrace] -> Speaker -> IOArray Int VolumeCollection -> IO (IOArray Int VolumeCollection)
channelForAllRayTraces _ [] _ arr = return arr
channelForAllRayTraces sampleRate (x:xs) speaker arr = do
    channelForRayTrace sampleRate x speaker arr
    channelForAllRayTraces sampleRate xs speaker arr

createChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO [VolumeCollection]
createChannel samples sampleRate raytraces speaker = do
    t <- newArray (0, samples) (pure 0)
    u <- channelForAllRayTraces sampleRate raytraces speaker t
    getElems u

splitBands :: [VolumeCollection] -> C3 [Flt]
splitBands = foldr (abop (:)) (pure [])

filterBands :: C3 [Flt] -> Flt -> C3 [Flt]
filterBands (C3 b0 b1 b2) sampleRate = C3 f0 f1 f2
    where   f0 = lopass b0 sampleRate 200
            f1 = bandpass b1 sampleRate 200 2000
            f2 = hipass b2 sampleRate 2000

compileBands :: C3 [Flt] -> [Flt]
compileBands (C3 a b c) = zipWith3 (\ a b c -> a + b + c) a b c

lopass :: [Flt] -> Flt -> Flt -> [Flt]
lopass band sampleRate cutoff = out
    where (out, _) = lopassWorker (1 - exp (-2 * pi * cutoff / sampleRate)) band

lopassWorker :: Flt -> [Flt] -> ([Flt], Flt)
lopassWorker a0 = foldr (worker a0) ([], 0) 
    where   worker a0 unit (out, state) = (new:out, new)
                where   new = state + a0 * (unit - state) 

hipass :: [Flt] -> Flt -> Flt -> [Flt]
hipass band sampleRate cutoff = zipWith (-) band (lopass band sampleRate cutoff)

bandpass :: [Flt] -> Flt -> Flt -> Flt -> [Flt]
bandpass band sampleRate lo = lopass (hipass band sampleRate lo) sampleRate

normalizeChannel :: [Flt] -> [Flt]
normalizeChannel channel = map (* (0.99 / (maximumBy (compare . abs) channel))) channel

processChannel sr channel = normalizeChannel $ hipass (compileBands (filterBands (splitBands channel) sr)) sr 20

createAndProcessChannel :: Int -> Flt -> [RayTrace] -> Speaker -> IO [Flt]
createAndProcessChannel samples sr raytraces speaker = do 
    channel <- createChannel samples sr raytraces speaker
    return $ processChannel sr channel

createAllChannels :: Int -> Flt -> [RayTrace] -> [Speaker] -> IO [[Flt]]
createAllChannels samples sampleRate raytraces = 
    mapM (createAndProcessChannel samples sampleRate raytraces) 

