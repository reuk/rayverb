module Main where

import Prelude hiding (writeFile)

import Numerical
import Vec3
import Scene
import Primitive
import Microphone
import Material
import Container

import System.Environment

import Data.Aeson

import Data.ByteString.Lazy (writeFile)

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

microphone :: Microphone
microphone = Mic $ Vec3 (-5) (-5) (-5)

tracer :: [Primitive] -> Microphone -> Int -> Flt -> String -> IO ()
tracer prims mic rays threshold filename = do
    r <- traceMic prims mic rays threshold
    writeFile filename $ encode r

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> tracer primitives microphone 1000 0.01 $ head args
        _ -> putStrLn "program takes one argument"
