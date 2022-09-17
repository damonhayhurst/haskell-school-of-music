import Euterpea

properRow :: Music Pitch -> Bool
properRow x = let pitches = [0..11]
                  l@(Prim (Note _ _) : _) = lineToList x
                  pitchInList [] (Prim (Note _ (_, _)): _) = False
                  pitchInList ps [] = null ps
                  pitchInList ps (Prim (Note _ (r, _)) : ns) = pitchInList (filter (/= pcToInt r) ps) ns
              in pitchInList pitches l

x = line [c 4 en, cs 5 en, d 5 en, ds 5 en, e 4 en, f 3 en, fs 3 en, g 3 en, rest en, gs 4 en, a 3 en, as 3 qn, b 3 en]
y = line [c 4 en, cs 5 en, d 5 en, ds 5 en, e 4 en, f 3 en, fs 3 en, g 3 en, gs 4 en]
z = line [c 4 en, cs 5 en, d 5 en, ds 5 en, e 4 en, f 3 en, fs 3 en, g 3 en, gs 4 en, a 3 en, as 3 en, b 3 en, b 3 en]