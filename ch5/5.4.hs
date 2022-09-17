import Euterpea

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(ap1, ap2) | ap1 <- aps1, ap2 <- aps2, (\a1 a2 -> a1 - a2 > 2 && a1 - a2 < 8) ap1 ap2] 

-- q = apPairs [52, 35, 55, 55] [45, 32, 60, 50]

apChords :: [(AbsPitch, AbsPitch)] -> Music Pitch
apChords apcs = line [chord [note d1 (pitch a1), note en (pitch a2)] | (a1, a2) <- apcs, let d1 = if even a1 then sn else en]
