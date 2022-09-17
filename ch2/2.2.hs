import Euterpea


data BluesPitchClass = Ro | MT | Fo | Fi | MS
type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o); mt o d = note d (MT, o)
fo o d = note d (Fo, o); fi o d = note d (Fi, o);
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (Ro, o))) = Prim (Note d (C, o))
fromBlues (Prim (Note d (MT, o))) = Prim (Note d (Ef, o))
fromBlues (Prim (Note d (Fo, o))) = Prim (Note d (F, o))
fromBlues (Prim (Note d (Fi, o))) = Prim (Note d (G, o))
fromBlues (Prim (Note d (MS, o))) = Prim (Note d (Bf, o))
fromBlues (Prim (Rest d)) = Prim (Rest d)
fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues (Modify ctrl m) = Modify ctrl (fromBlues m)

melody = fromBlues $ mt 4 qn :+: fi 4 qn :+: ro 4 wn 