import Euterpea

mordent :: Int -> Music Pitch -> Music Pitch
mordent i (Prim (Note d p)) = note (d/8) p :+: note (d/8) (trans i p) :+: note (d*3/4) p
mordent i (Modify (Tempo r) m) = tempo r (mordent i m)
mordent i (Modify c m) = Modify c (mordent i m)
mordent _ _ = 
    error "mordent: input must be a single note"

turn :: Music Pitch -> Music Pitch
turn (Prim (Note d p)) = let fifth = d / 5 
                         in note fifth p :+: note fifth (trans 1 p) :+: note fifth p :+: note fifth (trans (-1) p) :+: note fifth p
turn (Modify (Tempo r) m) = tempo r (turn m)
turn (Modify c m) = Modify c (turn m)
turn _ =
    error "turn: input must be a single note" 

appoggiatura :: Music Pitch -> [Music Pitch] -> Music Pitch
appoggiatura (Prim (Note d p)) (Prim (Note da pa): as) = note da pa :+: appoggiatura (note (d - da) p) as 
appoggiatura (Prim (Note d p)) (Prim (Rest da): as) = rest da :+: appoggiatura (note (d - da) p) as
appoggiatura (Prim (Note d p)) [] = note d p
appoggiatura _ _ =
    error "appoggiatura: can only add a note or rest to a note"