module Main where

import Impulse
import Primitive
import Material
import Vec3
import Mic

--  for xml generation
import Data.Monoid
import Data.Text (pack)
import qualified Data.ByteString.Lazy as BSL
import Text.XML.Generator

a   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (50)
b   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (-50)
c   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (10)
d   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (-10)
e   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (10)
f   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (-10)

s   = newSphere (Material 0.9 0.9) True (Vec3 40 5 5) 1

p   = [a, b, c, d, e, f, s]

m   = Mic (Vec3 10 10 10)

genImpulseElem :: Impulse -> Xml Elem
genImpulseElem impulse = xelem (pack "i") $ 
    xattrs [
        xattr (pack "amp") (pack $ show $ amplitude impulse),
        xattr (pack "pos") (pack $ show $ samplePosition impulse)]

genRayElem :: (Vec3 Double, [Impulse]) -> Xml Elem
genRayElem ((Vec3 x y z), impulses) = xelem (pack "r") $ 
    xattrs [
        (xattr (pack "x") (pack $ show x)),
        (xattr (pack "y") (pack $ show y)),
        (xattr (pack "z") (pack $ show z))] <#> (xelems $ map genImpulseElem impulses)

genTraceElem :: [(Vec3 Double, [Impulse])] -> Xml Elem
genTraceElem = xelems . map genRayElem

main = BSL.putStrLn (xrender xml)
    where xml = doc defaultDocInfo $ xelem (pack "impulses") $ genTraceElem $ traceMic p m 100000