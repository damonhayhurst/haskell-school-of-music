import Euterpea

retroPitches :: Music Pitch -> Music Pitch
retroPitches x = let p = lineToList x
                     r = reverse p
                     repitch [] [] nps = nps
                     repitch (Prim (Note d _): ps) (Prim (Note _ rp): rps) nps = repitch ps rps (nps ++ [note d rp])
                     repitch (Prim (Rest d): ps) rps nps = repitch ps rps (nps ++ [rest d])
                     repitch ps (_:rps) nps = repitch ps rps nps
                 in line(repitch p r [])