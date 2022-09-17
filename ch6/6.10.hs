import Euterpea

-- retro :: Music a -> Music a
-- retro n@(Prim _) = n
-- retro (Modify c m) = Modify c (retro m)
-- retro (m1 :+: m2) = retro m2 :+: retro m1
-- retro (m1 :=: m2) =
--     let d1 = dur m1
--         d2 = dur m2
--     in if d1 > d2 
--        then retro m1 :=: (rest (d1 - d2) :+: retro m2)
--        else (rest (d2 - d1) :+: retro m1) :=: retro m2

 
foldRetro :: Music a -> Music a
foldRetro = mFold Prim swapM shortenM Modify where
    recFold = mFold Prim swapM shortenM Modify 
    swapM m1 m2 = (:+:) (recFold m2) (recFold m1)
    shortenM m1 m2 = let d1 = dur m1
                         d2 = dur m2
                     in if d1 > d2
                        then recFold m1 :=: (rest (d1 - d2) :+: recFold m2)
                        else (rest (d2 - d1) :+: recFold m1) :=: recFold m2