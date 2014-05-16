module Directionable where

import Numerical

class Directionable c where
    direction :: c -> Vec
