{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Euterpea
import Debug.Trace
import GHC.Integer (Integer)

-- hNote :: Dur -> Pitch -> Music Pitch  
-- hNote d p = note d p :=: note d (trans (-3) p)

hNote :: Dur -> Pitch -> Int -> Music Pitch
hNote d p i = note d p :=: note d (trans i p)

-- mel p1 = note qn p1 :=: note qn (trans (-3) p1)

mel :: Music Pitch
mel = hList qn [(C, 2), (C, 3), (C, 4)] (-3)

hList :: Dur -> [Pitch] -> Int -> Music Pitch
hList d ps i = foldr (\ p -> (:+:) (hNote d p i)) (rest 0) ps

---Chapter 2

t251::Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor


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

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) =  let i = pitch (absPitch p  + ap)
                                in Prim (Note d i)
transM ap (Prim (Rest d)) = Prim (Rest d)
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify ctrl m) = Modify ctrl (transM ap m)

---Chapter 3

f1 :: Int -> [Pitch] -> [Pitch]
f1 i [] = []
f1 i ps = map (trans i) ps

f2 :: [Dur] -> [Music a]
f2 [] = []
f2 r = map rest r

f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 ps =  let fn (Prim (Note d p )) = Prim (Note (d/2) p ) :+: rest (d/2)
         in map fn ps

xs = [1, 2, 3] :: [Integer]
ys :: [Integer -> Integer]
ys = map (+) xs

simple x y z = x * (y+ z)

applyEach fn v = let f5 val function = function val
                 in map (f5 v) fn

applyAll [] v = v
applyAll (fn:fns) v = fn (applyAll fns v)

length1 :: [a] -> Integer 
length1 x = let count c = c + 1
                len c [] = c
                len c (x : xs) = len (count c) xs
            in len 0 x

doubleEach :: [Integer] -> [Integer]
doubleEach x = let double d = d*2
                    in map double x

pairAndOne :: [Integer] -> [(Integer, Integer)]
pairAndOne x = let andOne n = (n, n+1)
                in map andOne x

addEachPair :: [(Integer, Integer)] -> [Integer]
addEachPair x = let addPair (n1, n2) = n1 + n2
                in map addPair x

addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)
addPairsPointwise x = let add (a1, a2) (c, d) = (a1 + c, a2 + d)
                          addPair a [] = a
                          addPair a (x:xs) = addPair (add a x) xs
                      in addPair (0,0) x

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]  
fuse d p = zipWith ($) p d

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "maxAbsPitch: empty list"
maxAbsPitch x = maximum x 

-- maxAbsPitchR :: [AbsPitch] -> AbsPitch -> AbsPitch
-- maxAbsPitchR [] 0 = error "maxAbsPitchR: empty list"
-- maxAbsPitchR [] m = m
-- maxAbsPitchR (x:xs) m = maxAbsPitchR xs (max x m)

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "minAbsPitch: empty list"
minAbsPitch x = minimum x

-- minAbsPitchR :: [AbsPitch] -> AbsPitch -> AbsPitch
-- minAbsPitchR [] 0 = error "minAbsPitchR: empty list"
-- minAbsPitchR [] m = m
-- minAbsPitchR (x:xs) m = minAbsPitchR xs (min x m)

chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let scale p1 p2 = [(absPitch p1) .. (absPitch p2)]
              in line (map (note qn . pitch) (scale p1 p2))  

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p [] = note qn p
mkScale p (int:ints) = note qn p :+: mkScale(trans int p) ints

frere = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn 
dormez = e 4 qn :+: f 4 qn :+: g 4 qn :+: rest qn
sonnez = g 4 en :+: a 4 qn :+: g 4 qn :+: f 4 en :+: e 4 qn :+: c 4 qn
dong = c 4 qn :+: g 3 qn :+: c 4 qn :+: rest qn

song = frere :+: frere :+: dormez :+: dormez :+: sonnez :+: sonnez :+: dong :+: dong

-- encrypt = foldr toEnum Int

twice :: (a -> a) -> a -> a
twice f a = f(f a) 

power :: (a -> a) -> Int -> a -> a
power f n a = if n==0 then a else f(power f (n-1) a)

-- fix :: (a -> a) -> a -> a

fix :: (a -> a) -> a
fix f = f(fix f)

remainder :: Integer -> Integer -> Integer
remainder a b = fix (\f n -> if n < b then n else n - b) a

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(ap1, ap2) | ap1 <- aps1, ap2 <- aps2, (\a1 a2 -> a1 - a2 > 2 && a1 - a2 < 8) ap1 ap2] 

q = apPairs [52, 35, 55, 55] [45, 32, 60, 50]

apChords :: [(AbsPitch, AbsPitch)] -> [Music Pitch]
apChords apcs = line [chord [note d1 (pitch a1), note qn (pitch a2)] | (a1, a2) <- apcs, let d1 = if even a1 then sn else en]

