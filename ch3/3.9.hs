import Euterpea

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]  
fuse d p = zipWith ($) p d