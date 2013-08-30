module Vec3 where

import Control.Applicative

data Vec3 a = Vec3 (a, a, a) deriving (Eq, Show)

instance Functor Vec3 where
    fmap f (Vec3 (x, y, z)) = Vec3 (f x, f y, f z)

instance Applicative Vec3 where
    pure x = Vec3 (x, x, x)
    (Vec3 (f, g, h)) <*> (Vec3(x, y, z)) = Vec3 (f x, g y, h z)

instance Num a => Num (Vec3 a) where
    (+)          = (<*>) . (<$>) (+)
    (-)          = (<*>) . (<$>) (-)
    (*)          = (<*>) . (<$>) (*)
    abs          = fmap abs
    signum       = fmap signum
    fromInteger  = pure . fromInteger

magnitude x = sqrt (a + b + c)
    where   (Vec3 (a, b, c)) = fmap (^ 2) x

distance a b = magnitude (a - b)

normalize a = fmap (/ n) a
    where   n = magnitude a

dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (xv, yv, zv)
    where   xv  = (y1 * z2) - (z1 * y2)
            yv  = (z1 * x2) - (x1 * z2)
            zv  = (x1 * y2) - (y1 * x2)
