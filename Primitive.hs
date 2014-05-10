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

constructTriangle :: Surface -> Vec -> Vec -> Vec -> Primitive
constructTriangle mat v0 v1 v2 = 
    Triangle mat v0 v1 v2 e0 e1 (normalize (cross e0 e1))
    where   e0 = v1 - v0
            e1 = v2 - v0

findNormal :: Primitive -> Vec -> Vec
findNormal (Sphere _ _ orig _) point = normalize $ point - orig
findNormal (Plane _ norm _) _ = norm
findNormal (Triangle _ _ _ _ _ _ norm) _ = norm

intersection :: Primitive -> Ray -> Maybe Flt
intersection (Sphere _ _ orig rad) (Ray position direction) = 
    if disc > 0 
        then Just $ -(b + sqrt disc) / (2 * a)
        else Nothing
    where   disc = (b * b) - (4 * a * c)
            a = dot direction direction
            b = 2 * dot direction pSubOrigin
            c = (dot pSubOrigin pSubOrigin) - (rad * rad)
            pSubOrigin = position - orig
intersection (Plane _ norm dist) (Ray position direction) =
    if det /= 0 && e > 0
        then Just e
        else Nothing
    where   det = dot norm direction
            e = (dist - dot norm position) / det
intersection (Triangle _ v0 _ _ e0 e1 _) (Ray position direction) 
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
    if null filtered then Nothing else Just prim
    where   zipped = zip primitives (map (\ x -> intersection x r) primitives)
            filtered = filter (\ (_, x) -> isJust x && fromJust x > 0.00001) zipped
            (prim, _) = minimumBy (\ (_, x) (_, y) -> compare x y) filtered
