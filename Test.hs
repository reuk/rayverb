module Test where

import Impulse
import Primitive
import Mic
import Vec3
import Material
import Reflection
import Ray

a   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (100)
b   = newPlane (Material 0.9 0.9) (Vec3 1 0 0) (-100)
c   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (100)
d   = newPlane (Material 0.9 0.9) (Vec3 0 1 0) (-100)
e   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (100)
f   = newPlane (Material 0.9 0.9) (Vec3 0 0 1) (-100)

s   = newSphere (Material 0.9 0.9) True (Vec3 50 50 50) 1

p   = [a, b, c, d, e, f, s]

mic = newCardioid (Vec3 (-50) (-50) (-50)) (Vec3 1 2 3)

r   = traceMic p 1 mic

q   = trace p (Ray (Vec3 (-50) (-50) (-50)) (Vec3 1 3 2) 1)
