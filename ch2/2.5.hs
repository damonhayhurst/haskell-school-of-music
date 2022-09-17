import Euterpea

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) =  let i = pitch (absPitch p  + ap)
                                in Prim (Note d i)
transM ap (Prim (Rest d)) = Prim (Rest d)
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify ctrl m) = Modify ctrl (transM ap m)