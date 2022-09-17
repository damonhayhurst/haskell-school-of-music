import Euterpea

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "maxAbsPitch: empty list"
maxAbsPitch x = maximum x 

-- maxAbsPitchR :: [AbsPitch] -> AbsPitch -> AbsPitch
-- maxAbsPitchR [] 0 = error "maxAbsPitchR: empty list"
-- maxAbsPitchR [] m = m
-- maxAbsPitchR (x:xs) m = maxAbsPitchR xs (max x m)

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "minAbsPitch: empty list"
minAbsPitch x = minimum x

-- minAbsPitchR :: [AbsPitch] -> AbsPitch -> AbsPitch
-- minAbsPitchR [] 0 = error "minAbsPitchR: empty list"
-- minAbsPitchR [] m = m
-- minAbsPitchR (x:xs) m = minAbsPitchR xs (min x m)
