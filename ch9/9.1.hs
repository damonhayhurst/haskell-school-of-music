import Euterpea

myPasHandler (Dyn (Crescendo x)) pf =
    let step = (x / (fromIntegral (length pf - 1)))
        start = fromIntegral (eVol (head pf))
        end = start + x
    in zipWith (\e v -> e {eVol = round v}) pf [start,(start + step)..end]