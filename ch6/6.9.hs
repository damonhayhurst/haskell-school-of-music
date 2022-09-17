import Euterpea

scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
scaleVolume s = mMap (\(p, v) -> (p, round (s * fromIntegral v)))