module Positionable where

import Numerical

class Positionable c where
    position :: c -> Vec
