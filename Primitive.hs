module Primitive where

import Numerical
import Vec3
import Ray
import Material

import Data.List
import Data.Maybe
import Control.Applicative

data Primitive = Sphere     {   surface :: Surface
                            ,   isSource :: Bool
                            ,   origin :: Vec
                            ,   radius :: Flt
                            } 
               | Plane      {   surface :: Surface
                            ,   normal :: Vec
                            ,   distance :: Flt
                            } 
               | Triangle   {   surface :: Surface
                            ,   vert0 :: Vec
                            ,   vert1 :: Vec
                            ,   vert2 :: Vec
                            ,   edge0 :: Vec
                            ,   edge1 :: Vec
                            ,   normal :: Vec
                            }   deriving (Eq, Show)

constructSphere :: Surface -> Bool -> Vec -> Flt -> Primitive
constructSphere surface isSource origin radius = 
    Sphere surface isSource origin radius

constructPlane :: Surface -> Vec -> Flt -> Primitive
constructPlane surface normal distance =
    Plane surface normal distance

constructTriangle :: Surface -> Vec -> Vec -> Vec -> Primitive
constructTriangle surface v0 v1 v2 = 
    Triangle surface v0 v1 v2 e0 e1 (normalize (cross e0 e1))
    where   e0 = v1 - v0
            e1 = v2 - v0

findNormal :: Primitive -> Vec -> Vec
findNormal (Sphere _ _ origin _) point = normalize $ point - origin
findNormal (Plane _ normal _) point = normal
findNormal (Triangle _ _ _ _ _ _ normal) point = normal

intersection :: Primitive -> Ray -> Maybe Flt
intersection (Sphere _ _ origin radius) (Ray position direction) = 
    if disc > 0 
        then Just $ -(b + sqrt disc) / (2 * a)
        else Nothing
    where   disc = (b * b) - (4 * a * c)
            a = dot direction direction
            b = 2 * dot direction pSubOrigin
            c = (dot pSubOrigin pSubOrigin) - (radius * radius)
            pSubOrigin = position - origin
intersection (Plane _ normal distance) (Ray position direction) =
    if det /= 0 && e > 0
        then Just e
        else Nothing
    where   det = dot normal direction
            e = (distance - dot normal position) / det
intersection (Triangle _ v0 v1 v2 e0 e1 normal) (Ray position direction) 
    | -epsilon < det && det < epsilon   = Nothing
    | u < 0 || 1 < u                    = Nothing
    | v < 0 || 1 < u + v                = Nothing
    | otherwise                         = Just $ invdet * dot e1 qvec
    where   epsilon     = 0.000001
            det         = dot e0 pvec
            pvec        = cross direction e1

            u           = invdet * dot tvec pvec
            tvec        = position - v0
            invdet      = 1 / det

            v           = invdet * dot direction qvec
            qvec        = cross tvec e0

reflectFromPrimitive :: Primitive -> Ray -> Ray
reflectFromPrimitive prim ray@(Ray position direction) = reflectRay ray nor pos
    where   nor         = findNormal prim pos
            pos         = position + (direction * dist)
            dist        = pure $ fromMaybe 0 inter
            inter       = intersection prim ray

closest :: Ray -> [Primitive] -> Maybe Primitive
closest r primitives =
    if null filtered then Nothing else Just closest
    where   zipped = zip primitives (map (\ x -> intersection x r) primitives)
            filtered = filter (\ (_, x) -> isJust x && fromJust x > 0.00001) zipped
            (closest, _) = minimumBy (\ (_, x) (_, y) -> compare x y) filtered
