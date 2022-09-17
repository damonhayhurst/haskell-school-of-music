import Euterpea


-- applyEach fn v = let f5 val function = function val
--                  in map (f5 v) fn
applyEach fn v = map (\f -> f v) fn


-- doubleEach x = let double d = d*2
--                     in map double x
doubleEach = map (*2)


-- pairAndOne x = let andOne n = (n, n+1)
--                 in map andOne x

pairAndOne = map (\n -> (n, n+1))

-- addEachPair x = let addPair (n1, n2) = n1 + n2
--                 in map addPair x

addEachPair = map (uncurry (+))

-- addPairsPointwise x = let add (a1, a2) (c, d) = (a1 + c, a2 + d)
--                           addPair a [] = a
--                           addPair a (x:xs) = addPair (add a x) xs
--                       in addPair (0,0) x

addPairsPointwise x = let addPair a [] = a
                          addPair a (x:xs) = addPair ((\(a1, a2) (c, d) -> (a1 + c, a2 + d)) a x) xs
                      in addPair (0,0) x

-- twice f a = f(f a) 

twice f = f . f

-- addDur d ns = let f n = n d
--                 in line (map f ns)

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line $ map (\n -> n d) ns