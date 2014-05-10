module ApplicativeBinaryOp where

import Control.Applicative

abop :: Applicative f => (a1 -> a -> b) -> f a1 -> f a -> f b
abop f = (<*>) . (<$>) f
-- abop f a b = a `seq` b `seq` (f <$> a <*> b)
