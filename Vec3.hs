module Vec3 where

import Control.Applicative

data Vec3 a = Vec3 a a a
    deriving (Eq, Show)

sqrMagnitude (Vec3 x y z) = x * x + y * y + z * z

magnitude v = sqrt $ sqrMagnitude v

normalize v = (/ magnitude v) <$> v

dot (Vec3 ax ay az) (Vec3 bx by bz) = ax * bx + ay * by + az * bz

cross (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ay * bz - by * az)
                                             (az * bx - az * bx)
                                             (ax * by - bx * ay)

reflect n d = d - (n * pure (2 * dot n d))

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure x = Vec3 x x x
    Vec3 f g h <*> Vec3 x y z = Vec3 (f x) (g y) (h z)

instance (Num a) => Num (Vec3 a) where
    (+) = (<*>) . (<$>) (+)
    (-) = (<*>) . (<$>) (-)
    (*) = (<*>) . (<$>) (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
