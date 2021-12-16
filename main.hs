import Euterpea 

-- hNote :: Dur -> Pitch -> Music Pitch  
-- hNote d p = note d p :=: note d (trans (-3) p)

hNote :: Dur -> Pitch -> Int -> Music Pitch  
hNote d p i = note d p :=: note d (trans i p)

-- mel p1 = note qn p1 :=: note qn (trans (-3) p1)

mel :: Music Pitch
mel = hList qn [(C, 2), (C, 3), (C, 4)] (-3)

hList :: Dur -> [Pitch] -> Int -> Music Pitch
hList d [] i = rest 0 
hList d (p : ps) i = hNote d p i :+: hList d ps i

t251::Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn 
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn 
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn 
       in dMinor :+: gMajor :+: cMajor


major :: (PitchClass, Octave) -> Int -> (PitchClass, Octave)
major p d = let majorSequence = [0, 2, 2, 1, 2, 2, 2, 1]
            in trans (majorSequence !! (d-1)) p

majorTriad :: Dur -> (PitchClass, Octave) -> Music Pitch 
majorTriad d p = note d p :=: note d (trans 4 p) :=: note d (trans 7 p)

minorTriad :: Dur -> (PitchClass, Octave) -> Music Pitch
minorTriad d p = note d p :=: note d (trans 3 p) :=: note d (trans 7 p)

twoFiveOne :: Dur -> (PitchClass, Octave) -> Music Pitch
twoFiveOne d p = let two = major p 2; five = major p 5; one = major p 1;
                 in minorTriad d two :+: majorTriad d five :+: majorTriad (2*d) one

