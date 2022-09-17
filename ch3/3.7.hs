length1 :: [a] -> Integer 
length1 x = let len c [] = c
                len c (x : xs) = len (c + 1) xs
            in len 0 x