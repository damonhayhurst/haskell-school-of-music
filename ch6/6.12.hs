s1 = [1, 5, 3, 6, 5, 0, 1, 1]

toIntervals :: [Integer] -> [[Integer]]
toIntervals n = let intervals n = zipWith (-) (tail n) n
                    repeat [] s = s
                    repeat n s = repeat (intervals n) (s ++ [n])
                in repeat n []

getHeads :: [[Integer]] -> [Integer]
getHeads = map head

intervalClosure :: [Integer] -> [Integer]
intervalClosure = reverse . getHeads . toIntervals