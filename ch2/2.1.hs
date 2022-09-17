import Euterpea


major :: (PitchClass, Octave) -> Int -> (PitchClass, Octave)
major p d = let majorSequence = [0, 2, 4, 5, 7, 9, 11, 12]
            in trans (majorSequence !! (d-1)) p

majorTriad :: Dur -> (PitchClass, Octave) -> Music Pitch
majorTriad d p = note d p :=: note d (trans 4 p) :=: note d (trans 7 p)

minorTriad :: Dur -> (PitchClass, Octave) -> Music Pitch
minorTriad d p = note d p :=: note d (trans 3 p) :=: note d (trans 7 p)

twoFiveOne :: Dur -> (PitchClass, Octave) -> Music Pitch
twoFiveOne d p = let two = major p 2; five = major p 5; one = major p 1;
                 in minorTriad d two :+: majorTriad d five :+: majorTriad (2*d) one