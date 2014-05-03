module ApplicativeBinaryOp where

import Control.Applicative

abop f = (<*>) . (<$>) f
