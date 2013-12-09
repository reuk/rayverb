module Main where

import Impulse
import Primitive
import Material
import Vec3
import Mic
import Container

--  for xml generation
import Data.Monoid
import Data.Text (pack)
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Generator

a   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 1 0 0) (50)
b   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 1 0 0) (-50)
c   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 0 1 0) (50)
d   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 0 1 0) (-50)
e   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 0 0 1) (50)
f   = newPlane (C3  (Material 0.75 0.8)
                    (Material 0.9 0.9) 
                    (Material 0.5 0.65)) (Vec3 0 0 1) (-50)

s   = newSphere (C3 (Material 0.95 0.9)
                    (Material 0.95 0.9) 
                    (Material 0.95 0.9)) True (Vec3 40 5 5) 1

p   = [a, b, c, d, e, f, s]

m   = Mic (Vec3 10 10 10)

genImpulseElem :: Impulse -> Xml Elem
genImpulseElem impulse = xelem (pack "i") $ 
    xattrs [
        xattr (pack "a1") (pack $ show a1),
        xattr (pack "a2") (pack $ show a2),
        xattr (pack "a3") (pack $ show a3),
        xattr (pack "p") (pack $ show $ samplePosition impulse)]
    where   (C3 a1 a2 a3) = amplitude impulse

genRayElem :: (Vec3 Double, [Impulse]) -> Xml Elem
genRayElem ((Vec3 x y z), impulses) = xelem (pack "r") $ 
    xattrs [
        (xattr (pack "x") (pack $ show x)),
        (xattr (pack "y") (pack $ show y)),
        (xattr (pack "z") (pack $ show z))] <#> (xelems $ map genImpulseElem impulses)

genTraceElem :: [(Vec3 Double, [Impulse])] -> Xml Elem
genTraceElem = xelems . map genRayElem

main = BSL.putStrLn (xrender xml)
    where xml = doc defaultDocInfo $ xelem (pack "impulses") $ genTraceElem $ traceMic p m 10000