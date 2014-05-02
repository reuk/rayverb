module Speaker where

import Numerical
import Vec3
import Directionable

data Speaker = Speaker Vec Flt

instance Directionable Speaker where
    direction (Speaker x _) = x

directionality (Speaker _ x) = x

constructSpeaker :: Flt -> Flt -> Flt -> Speaker
constructSpeaker inclination azimuth =
    Speaker (polarToCartesian inclination azimuth) 

attenuation :: Speaker -> Vec -> Flt
attenuation (Speaker sDir directionality) rDir =
    (1 - directionality) + 
    (directionality * dot (normalize rDir) (normalize sDir))

inclination :: Speaker -> Flt
inclination (Speaker (Vec3 x y z) _) = acos y

azimuth :: Speaker -> Flt
azimuth (Speaker (Vec3 x y z) _) = atan2 z x

polarToCartesian :: Flt -> Flt -> Vec
polarToCartesian inclination azimuth = 
    Vec3    ((sin inclination) * (cos azimuth))
            (cos inclination)
            ((sin inclination) * (sin azimuth))

