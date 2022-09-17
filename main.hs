{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map once" #-}
import Euterpea
import Debug.Trace
import GHC.Integer (Integer)
import GHC.Real (Integral, Fractional)
import GHC.Num (subtract)
import Data.Bool (Bool)

hNote :: Dur -> Pitch -> Music Pitch  
hNote d p = note d p :=: note d (trans (-3) p)

-- hNote :: Dur -> Pitch -> Int -> Music Pitch
-- hNote d p i = note d p :=: note d (trans i p)

-- mel p1 = note qn p1 :=: note qn (trans (-3) p1)

-- mel :: Music Pitch
-- mel = hList qn [(C, 2), (C, 3), (C, 4)] (-3)

-- hList :: Dur -> [Pitch] -> Int -> Music Pitch
-- hList d ps i = foldr (\ p -> (:+:) (hNote d p i)) (rest 0) ps

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

-- applyEach fn v = let f5 val function = function val
--                  in map (f5 v) fn
applyEach fn v = map (\f -> f v) fn



applyAll [] v = v
applyAll (fn:fns) v = fn (applyAll fns v)



length1 :: [a] -> Integer 

-- length1 x = let count c = c + 1
--                 len c [] = c
--                 len c (x : xs) = len (count c) xs
--             in len 0 x

length1 x = let len c [] = c
                len c (x : xs) = len (c + 1) xs
            in len 0 x


doubleEach :: [Integer] -> [Integer]

-- doubleEach x = let double d = d*2
--                     in map double x
doubleEach = map (*2)


pairAndOne :: [Integer] -> [(Integer, Integer)]

-- pairAndOne x = let andOne n = (n, n+1)
--                 in map andOne x

pairAndOne = map (\n -> (n, n+1))


addEachPair :: [(Integer, Integer)] -> [Integer]

-- addEachPair x = let addPair (n1, n2) = n1 + n2
--                 in map addPair x

addEachPair = map (uncurry (+))

addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)

-- addPairsPointwise x = let add (a1, a2) (c, d) = (a1 + c, a2 + d)
--                           addPair a [] = a
--                           addPair a (x:xs) = addPair (add a x) xs
--                       in addPair (0,0) x

addPairsPointwise x = let addPair a [] = a
                          addPair a (x:xs) = addPair ((\(a1, a2) (c, d) -> (a1 + c, a2 + d)) a x) xs
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

chrom p1 p2 = let scale p1 p2 = [absPitch p1 .. absPitch p2]
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

-- twice f a = f(f a) 

twice f = f . f


power :: (a -> a) -> Int -> a -> a

power f n a = if n==0 then a else f(power f (n-1) a)

-- fix :: (a -> a) -> a -> a

fix :: (a -> a) -> a
fix f = f(fix f)

remainder :: Integer -> Integer -> Integer
remainder a b = fix (\f n -> if n < b then n else n - b) a

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(ap1, ap2) | ap1 <- aps1, ap2 <- aps2, (\a1 a2 -> a1 - a2 > 2 && a1 - a2 < 8) ap1 ap2] 

-- q = apPairs [52, 35, 55, 55] [45, 32, 60, 50]

apChords :: [(AbsPitch, AbsPitch)] -> Music Pitch
apChords apcs = line [chord [note d1 (pitch a1), note en (pitch a2)] | (a1, a2) <- apcs, let d1 = if even a1 then sn else en]

hList d = line . map (hNote d)
-- unsolved

--5.6
-- addDur d ns = let f n = n d
--                 in line (map f ns)

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line $ map (\n -> n d) ns

addOneAndHalf = map ((/2) . (+1))
addOneAndHalfMapTwice xs = map (/2) (map (+1) xs)

f1Andf2 = (\x a -> map ($ a) x) (map (*) [1, 2, 3, 4, 5] ) 5


--- chapter 6


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

palin :: Music Pitch -> Bool
palin x = let getPitches (Prim (Note _ (p, o))) = (p, o)
              l = map getPitches (lineToList x)
          in l == reverse l


p = line [c 4 sn, cs 5 en, rest qn, d 5 en, rest en]
q = line [c 4 sn, cs 5 en, rest qn]

retroPitches :: Music Pitch -> Music Pitch
retroPitches x = let p = lineToList x
                     r = reverse p
                     repitch [] [] nps = nps
                     repitch (Prim (Note d _): ps) (Prim (Note _ rp): rps) nps = repitch ps rps (nps ++ [note d rp])
                     repitch (Prim (Rest d): ps) rps nps = repitch ps rps (nps ++ [rest d])
                     repitch ps (_:rps) nps = repitch ps rps nps
                 in line(repitch p r [])

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

allPerc :: Music Pitch
allPerc = instrument Percussion $ line $ map (\p -> note qn (pitch p)) [35..86]


trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i sDur (Prim (Note tDur p)) = if sDur >= tDur then note tDur p
                                    else note sDur p :+: trill (negate i) sDur (note (tDur - sDur) (trans i p))
trill i d (Modify (Tempo r) m) = tempo r (trill i (d * r) m)
trill i d (Modify c m) = Modify c (trill i d m)
trill _ _ _ = 
    error "trill: input must be a single note."  

trill' :: Int -> Dur -> Music Pitch -> Music Pitch
trill' i sDur m = trill (negate i) sDur (transpose i m)

trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m/fromIntegral nTimes) m

trilln' :: Int -> Int -> Music Pitch  -> Music Pitch
trilln' i nTimes m = trilln (negate i) nTimes (transpose i m)

roll :: Dur -> Music Pitch -> Music Pitch
rolln :: Int -> Music Pitch -> Music Pitch

roll dur m = trill 0 dur m
rolln nTimes m = trilln 0 nTimes m

funkGroove :: Music Pitch
funkGroove = let p1 = perc LowTom qn
                 p2 = perc AcousticSnare en
             in tempo 3 $ cut 8 $ forever
                ((p1 :+: qnr :+: p2  :+: qnr :+: p2 :+:
                  p1 :+: p1  :+: qnr :+: p2  :+: enr)
                  :=: roll en (perc ClosedHiHat 2))    

scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
scaleVolume s = mMap (\(p, v) -> (p, round (s * fromIntegral v)))

-- retro :: Music a -> Music a
-- retro n@(Prim _) = n
-- retro (Modify c m) = Modify c (retro m)
-- retro (m1 :+: m2) = retro m2 :+: retro m1
-- retro (m1 :=: m2) =
--     let d1 = dur m1
--         d2 = dur m2
--     in if d1 > d2 
--        then retro m1 :=: (rest (d1 - d2) :+: retro m2)
--        else (rest (d2 - d1) :+: retro m1) :=: retro m2

 
foldRetro :: Music a -> Music a
foldRetro = mFold Prim swapM shortenM Modify where
    recFold = mFold Prim swapM shortenM Modify 
    swapM m1 m2 = (:+:) (recFold m2) (recFold m1)
    shortenM m1 m2 = let d1 = dur m1
                         d2 = dur m2
                     in if d1 > d2
                        then recFold m1 :=: (rest (d1 - d2) :+: recFold m2)
                        else (rest (d2 - d1) :+: recFold m1) :=: recFold m2

insideOut :: Music a -> Music a
insideOut = mFold Prim (:=:) (:+:) Modify

-- insideOut $ c 4 qn :+: rest qn

x1 = g 4 qn :=: (c 4 en :+: d 4 en :+: e 4 en)
x2 = g 4 qn :=: tempo (3/2) (c 4 en :+: d 4 en :+: e 4 en)

rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n - 1) (f m))

run = rep (transpose 5) (offset tn) 8 (c 4 tn)
cascade = rep (transpose 4) (offset en) 8 run
cascades = rep id (offset sn) 2 cascade
final = cascades :+: retro cascades

run' = rep (offset tn) (transpose 5) 8 (c 4 tn)
cascade' = rep (offset en) (transpose 4) 8 run'
cascades' = rep (offset sn) id 2 cascade'
final' = cascades' :+: retro cascades'

-- toIntervals :: [Integer] -> [Integer]
-- toIntervals n = let intervals s [] = s
--                     intervals s (n:ns) = intervals (s ++ [head ns - n]) ns
--                 in intervals [] n

s1 = [1, 5, 3, 6, 5, 0, 1, 1]

toIntervals :: [Integer] -> [[Integer]]
toIntervals n = let intervals n = zipWith (-) (tail n) n
                    repeat [] s = s
                    repeat n s = repeat (intervals n) (s ++ [n])
                in repeat n []

getHeads :: [[Integer]] -> [Integer]
getHeads = map head

intervalClosure :: [Integer] -> [Integer]
intervalClosure = reverse . getHeads . toIntervals

data Color = Red
    | Blue
    | Green

instance Eq Color where
    Red == Red = True
    Blue == Blue = True
    Green == Green = True
    _ == _ = False

instance Ord Color where
    Blue <= Red = False
    Green <= Blue = False
    _ <= _ = True

instance Enum Color where
    fromEnum Red = 0
    fromEnum Blue = 1
    fromEnum Green = 2

    toEnum 0 = Red
    toEnum 1 = Blue
    toEnum 2 = Green

class Temporal a where
    durT :: a -> Dur
    cutT :: Dur -> a -> a
    removeT :: Dur -> a -> a

instance Temporal (Music a) where
    durT a = dur a
    cutT d a = cut d a
    removeT d a = remove d a

instance Temporal (Primitive a) where
    durT (Note d p) = d
    durT (Rest d) = d
    cutT d (Note nd p) = Note (nd - d) p
    cutT d (Rest rd) = Rest (rd - d)
    removeT d (Note nd p) = Note (nd - d) p
    removeT d (Rest rd) = Rest (rd - d)


instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
    f == f' = fmap f allArgs == map f' allArgs
        where allArgs = [minBound..maxBound] 

data Context a = Context {cTime :: PTime,
                          cPlayer :: Player a,
                          cInst :: InstrumentName,
                          cDur :: DurT,
                          cPch :: AbsPitch,
                          cVol :: Volume,
                          cKey :: (PitchClass, Mode)}
                deriving (Show)


type PMap a = PlayerName -> Player a
type PlayerName = String
data Player a = MkPlayer {pName :: PlayerName,
                          playNote :: NoteFun a,
                          interpPhrase :: PhraseFun a,
                          notatePlayer :: ()}
type NoteFun a = Context a -> Dur -> a -> Performance
type PhraseFun a = PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT)

instance Show a => Show(Player a) where 
    show p = "Player " ++ pName p


defPlayer :: Player Note1
defPlayer = MkPlayer
            {pName = "Default",
             playNote = defPlayNote defNasHandler,
             interpPhrase = defInterpPhrase defPasHandler}

defPlayNote :: (Context (Pitch, [a]) -> a -> MEvent -> MEvent) -> NoteFun (Pitch, [a])
defPlayNote nasHandler c@(Context cTime cPlayer cInst cDur cPch cVol cKey) d (p, nas) =
    let initEv = MEvent {
        eTime = cTime, 
        eInst = cInst,
        eDur = d * cDur, 
        eVol = cVol,
        ePitch = absPitch p + cPch,
        eParams = [] }
    in [foldr (nasHandler c) initEv nas]

defNasHandler :: Context a -> NoteAttribute -> MEvent -> MEvent
defNasHandler c (Volume v) ev = ev {eVol = v}
defNasHandler c (Params pms) ev = ev {eParams = pms}
defNasHandler _ _ ev = ev

defInterpPhrase :: 
    (PhraseAttribute -> Performance -> Performance) -> 
    (PMap a -> Context a -> [PhraseAttribute] -> 
    Music a -> (Performance, DurT))
defInterpPhrase pasHandler pm context pas m = 
    let (pf, dur) = perf pm context m
    in (foldr pasHandler pf pas, dur)

defPasHandler :: PhraseAttribute -> Performance -> Performance
defPasHandler (Dyn (Accent x)) = 
    map (\e -> e {eVol = round (x * fromIntegral (eVol e))})
defPasHandler (Art (Staccato x)) =
    map (\e -> e {eDur = x * eDur e})
defPasHandler (Art (Legato x)) =
    map (\e -> e {eDur = x * eDur e})
defPasHandler _ = id

hsomPerform :: PMap a -> Context a -> Music a -> Performance
hsomPerform pm c m = fst (perf pm c m)
perf :: PMap a -> Context a -> Music a -> (Performance, DurT)
perf pm c@Context{cTime = t, cPlayer = pl, cDur = dt, cPch = k} m =
    case m of 
        Prim(Note d p) -> (playNote pl c d p, d * dt)
        Prim(Rest d) -> ([], d * dt)
        m1 :+: m2 -> 
            let (pf1, d1) = perf pm c m1
                (pf2, d2) = perf pm (c {cTime = t + d1}) m2
            in (pf1 ++ pf2, d1 + d2)
        m1 :=: m2 ->
            let (pf1, d1) = perf pm c m1
                (pf2, d2) = perf pm c m2
            in (merge pf1 pf2, max d1 d2)
        Modify (Tempo r) m -> perf pm (c {cDur = dt / r}) m
        Modify (Transpose p) m -> perf pm (c {cPch = k + p}) m
        Modify (Instrument i) m -> perf pm (c {cInst = i}) m
        Modify (KeySig pc mo) m -> perf pm (c {cKey = (pc, mo)}) m
        Modify (Phrase pas) m -> interpPhrase pl pm c pas m
        Modify (Custom s) m -> 
            if take 7 s == "Player " then perf pm (c {cPlayer = pm $ drop 7 s}) m
            else perf pm c m


newPlayer :: Player (Pitch, [NoteAttribute])
newPlayer = defPlayer
    {pName = "NewPlayer",
     playNote = defPlayNote defNasHandler,
     interpPhrase = defInterpPhrase myPasHandler}

fancyPlayer :: Player (Pitch, [NoteAttribute])
fancyPlayer = MkPlayer{pName = "Fancy",
                       playNote = defPlayNote defNasHandler,
                       interpPhrase = fancyInterpPhrase,
                       notatePlayer = ()}
fancyInterpPhrase :: PhraseFun a
fancyInterpPhrase pm c [] m = perf pm c m
fancyInterpPhrase pm 
    c@Context{cTime = t, cPlayer = pl, cInst = i,
              cDur = dt, cPch = k, cKey = (pc, mode), cVol = v}
    (pa : pas) m =
    let pfd@(pf, dur) = fancyInterpPhrase pm c pas m
        loud x = fancyInterpPhrase pm c (Dyn (Loudness x): pas) m
        stretch x = let t0 = eTime (head pf); r = x/dur
                        upd(e@MEvent {eTime = t, eDur = d}) = let dt = t - t0
                                                                  t' = (1 + dt * r) * dt + t0
                                                                  d' = (1 + (2 * dt + d) * r) * d
                                                              in e {eTime = t', eDur = d'}
                    in (map upd pf, (1+x)* dur)
        inflate x = let t0 = eTime (head pf);
                        r = x / dur
                        upd(e@MEvent {eTime = t, eVol = v}) = e {eVol = round ((1 + (t - t0) * r) * fromIntegral v)}
                    in (map upd pf, dur)
    in case pa of 
        Dyn (Accent x) -> 
            (map (\e -> e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
        Dyn (StdLoudness l) ->
            case l of 
                PPP -> loud 40; PP -> loud 50; P -> loud 60
                MP -> loud 70; SF -> loud 80; MF -> loud 90
                NF -> loud 100; FF -> loud 110; FFF -> loud 120
        Dyn (Loudness x) -> fancyInterpPhrase pm
                            c {cVol = round x} pas m
        Dyn (Crescendo x) -> inflate x;
        Dyn (Diminuendo x) -> inflate (-x);
        Tmp (Ritardando x) -> stretch x;
        Tmp (Accelerando x) -> stretch (-x)
        Art (Staccato x) -> (map (\e -> e {eDur = x * eDur e}) pf, dur)
        Art (Legato x) -> (map  (\e -> e {eDur = x * eDur e}) pf, dur)
        Art (Slurred x) -> 
            let lastStartTime = foldr (\e t -> max (eTime e) t) 0 pf
                setDur e = if eTime e < lastStartTime
                           then e {eDur = x * eDur e}
                           else e
            in (map setDur pf, dur)
        Orn (Trill) -> let up = fancyInterpPhrase pm c {cKey = ((raise pc), mode), cDur = dt/4} pas m
                           down = fancyInterpPhrase pm c {cDur = dt/4} pas m
                           raise k = fst (pitch ((pcToInt k) + 1))
                        in foldr (\(xpf, xdur) (ypf, ydur) -> (xpf ++ ypf, xdur + ydur)) ([], 0) [down, up, down, up]
        Art _ -> pfd
        Orn _ -> pfd

myPasHandler (Dyn (Crescendo x)) pf =
    let step = (x / (fromIntegral (length pf - 1)))
        start = fromIntegral (eVol (head pf))
        end = start + x
    in zipWith (\e v -> e {eVol = round v}) pf [start,(start + step)..end]


