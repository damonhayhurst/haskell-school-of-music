import Euterpea

palin :: Music Pitch -> Bool
palin x = let getPitches (Prim (Note _ (p, o))) = (p, o)
              l = map getPitches (lineToList x)
          in l == reverse l


p = line [c 4 sn, cs 5 en, rest qn, d 5 en, rest en]
q = line [c 4 sn, cs 5 en, rest qn]