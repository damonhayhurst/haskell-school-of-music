import Euterpea


addDur :: Dur -> [Dur -> Music a] -> Music a

addDur d ns = let f n = n d
                in line (map f ns)
