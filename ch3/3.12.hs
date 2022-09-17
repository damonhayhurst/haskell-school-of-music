import Euterpea

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p [] = note qn p
mkScale p (int:ints) = note qn p :+: mkScale(trans int p) ints