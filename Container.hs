module Container where

import Control.Applicative
import Data.Foldable
import ApplicativeBinaryOp
import Control.DeepSeq

--

data C3 a = C3 a a a 
    deriving (Eq, Show)

instance Functor C3 where
    fmap f (C3 a b c) = C3 (f a) (f b) (f c)

instance Applicative C3 where
    pure x = C3 x x x
    (C3 f g h) <*> (C3 x y z) = C3 (f x) (g y) (h z)

instance Foldable C3 where
    foldr f b (C3 x y z) = f x $ f y $ f z b

instance (Num a) => Num (C3 a) where
    (+) = abop (+)
    (-) = abop (-)
    (*) = abop (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (NFData a) => NFData (C3 a) where
    rnf x = (fmap rnf x) `seq` ()

c3_0 :: C3 t -> t
c3_0 (C3 x _ _) = x

c3_1 :: C3 t -> t
c3_1 (C3 _ x _) = x

c3_2 :: C3 t -> t
c3_2 (C3 _ _ x) = x

--

data C4 a = C4 a a a a 
    deriving (Eq, Show)

instance Functor C4 where
    fmap f (C4 a b c d) = C4 (f a) (f b) (f c) (f d)

instance Applicative C4 where
    pure x = C4 x x x x
    (C4 f g h i) <*> (C4 x y z w) = C4 (f x) (g y) (h z) (i w)

instance Foldable C4 where
    foldr f b (C4 x y z w) = f x $ f y $ f z $ f w b

instance (Num a) => Num (C4 a) where
    (+) = abop (+)
    (-) = abop (-)
    (*) = abop (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (NFData a) => NFData (C4 a) where
    rnf x = (fmap rnf x) `seq` ()
