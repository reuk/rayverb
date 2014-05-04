module ApplicativeBinaryOp where

import Control.Applicative

-- abop f = (<*>) . (<$>) f
abop f a b = a `seq` b `seq` (f <$> a <*> b)
