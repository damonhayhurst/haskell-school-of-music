import Euterpea

chrom :: Pitch -> Pitch -> Music Pitch

chrom p1 p2 = let scale p1 p2 = [absPitch p1 .. absPitch p2]
              in line (map (note qn . pitch) (scale p1 p2))  