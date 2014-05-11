module Main where

import Numerical
import Vec3
import Scene
import Primitive
import Microphone
import Speaker
import Material
import Container
import Impulse

import System.Environment

import Text.JSON

instance (JSON a) => JSON (C3 a) where
    showJSON (C3 x y z) = JSObject $ toJSObject [("C3", JSArray [showJSON x, showJSON y, showJSON z])]

instance (JSON a) => JSON (Vec3 a) where
    showJSON (Vec3 x y z) = JSObject $ toJSObject [("x", showJSON x), ("y", showJSON y), ("z", showJSON z)]

instance JSON Impulse where
    showJSON (Impulse t a) = JSObject $ toJSObject [("p", showJSON t), ("a", showJSON a)]

instance JSON RayTrace where
    showJSON (RayTrace dir impulses) = JSObject $ toJSObject [("d", showJSON dir), ("i", showJSONs impulses)]

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
        1 -> tracer primitives microphone 10000 0.01 $ head args
        _ -> putStrLn "program takes one argument"
