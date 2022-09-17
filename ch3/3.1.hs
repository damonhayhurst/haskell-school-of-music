import Euterpea

f1 :: Int -> [Pitch] -> [Pitch]
f1 i [] = []
f1 i ps = map (trans i) ps

f2 :: [Dur] -> [Music a]
f2 [] = []
f2 r = map rest r

f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 ps =  let fn (Prim (Note d p )) = Prim (Note (d/2) p ) :+: rest (d/2)
         in map fn ps