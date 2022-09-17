doubleEach :: [Integer] -> [Integer]

doubleEach x = let double d = d*2
                    in map double x

pairAndOne :: [Integer] -> [(Integer, Integer)]

pairAndOne x = let andOne n = (n, n+1)
                in map andOne x

addEachPair :: [(Integer, Integer)] -> [Integer]

addEachPair x = let addPair (n1, n2) = n1 + n2
                in map addPair x

addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)

addPairsPointwise x = let add (a1, a2) (c, d) = (a1 + c, a2 + d)
                          addPair a [] = a
                          addPair a (x:xs) = addPair (add a x) xs
                      in addPair (0,0) x