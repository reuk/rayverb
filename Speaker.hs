module Speaker where

import Numerical
import Vec3
import Directionable

data Speaker = Speaker Vec Flt

instance Directionable Speaker where
    direction (Speaker x _) = x

directionality :: Speaker -> Flt
directionality (Speaker _ x) = x

constructSpeaker :: Flt -> Flt -> Flt -> Speaker
constructSpeaker inc azi =
    Speaker (polarToCartesian inc azi) 

attenuation :: Speaker -> Vec -> Flt
attenuation (Speaker sDir dir) rDir =
    (1 - dir) + 
    (dir * dot (normalize rDir) (normalize sDir))

inclination :: Speaker -> Flt
inclination (Speaker (Vec3 _ y _) _) = acos y

azimuth :: Speaker -> Flt
azimuth (Speaker (Vec3 x _ z) _) = atan2 z x

polarToCartesian :: Flt -> Flt -> Vec
polarToCartesian inc azi = 
    Vec3    ((sin inc) * (cos azi))
            (cos inc)
            ((sin inc) * (sin azi))

