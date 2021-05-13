module Ch6 where
import Euterpea
import Data.List(sort)

filterNote :: Music Pitch -> Bool
filterNote (Prim(Note d p)) = True 
filterNote _ = False 

primToPitchClassInt :: Music Pitch -> Int 
primToPitchClassInt (Prim(Note d p)) = absPitch p `mod` 12
primToPitchClassInt _ = undefined

primToAbsPitch :: Music Pitch -> Int 
primToAbsPitch (Prim(Note d p)) = absPitch p
primToAbsPitch _ = undefined

properRow :: Music Pitch -> Bool
properRow m = sort (map primToPitchClassInt $ filter filterNote $ lineToList m) == [0..11]

palin :: Music Pitch -> Bool 
palin m = 
    let pitches = map primToAbsPitch $ filter filterNote $ lineToList m
    in pitches == reverse pitches

combineNotes :: Music Pitch -> Music Pitch -> Music Pitch
combineNotes (Prim(Note d _)) (Prim(Note _ p)) = Prim(Note d p)
combineNotes (Prim(Rest d)) (Prim(Note _ p)) = Prim(Note d p)
combineNotes (Prim(Note d _)) (Prim(Rest _)) = Prim(Rest d)
combineNotes _ _ = undefined

retroPitches :: Music Pitch -> Music Pitch 
retroPitches m = 
    let linef = lineToList m
    in line $ zipWith combineNotes linef (reverse linef)

ssfMel :: Music Pitch
ssfMel = line (l1 ++ l2 ++ l3 ++ l4)
    where l1 = [trilln 2 5 (bf 6 en), ef 7 en, ef 6 en, ef 7 en]
          l2 = [bf 6 sn, c 7 sn, bf 6 sn, g 6 sn, ef 6 en, bf 5 en]
          l3 = [ef 6 sn, f 6 sn, g 6 sn, af 6 sn, bf 6 en, ef 7 en]
          l4 = [trill 2 tn (bf 6 qn), bf 6 sn, denr]
starsAndStripes :: Music Pitch
starsAndStripes = instrument Flute ssfMel

trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i sDur (Prim (Note tDur p)) =
    if sDur >= tDur then note tDur p
    else note sDur p :+:
        trill (negate i) sDur
            (note (tDur - sDur) (trans i p))
trill i d (Modify (Tempo r) m) = tempo r (trill i (d * r) m)
trill i d (Modify c m) = Modify c (trill i d m)
trill _ _ _ =
    error "trill: input must be a single note."

trill' :: Int -> Dur -> Music Pitch -> Music Pitch
trill' i sDur m = trill (negate i) sDur (transpose i m)

trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m/fromIntegral nTimes) m

trilln' :: Int -> Int -> Music Pitch -> Music Pitch
trilln' i nTimes m = trilln (negate i) nTimes (transpose i m)

roll :: Dur -> Music Pitch -> Music Pitch
rolln :: Int -> Music Pitch -> Music Pitch
roll = trill 0
rolln = trilln 0

grace :: Int -> Rational -> Music Pitch -> Music Pitch
grace n r (Prim (Note d p)) =
    note (r * d) (trans n p) :+: note ((1 - r) * d) p
grace n r _ =
    error "grace: can only add a grace note to a note"

grace2 :: Int -> Rational -> Music Pitch -> Music Pitch -> Music Pitch
grace2 n r (Prim (Note d1 p1)) (Prim (Note d2 p2)) =
    note (d1 - r * d2) p1 :+: note (r * d2) (trans n p2) :+: note d2 p2
grace2 _ _ _ _ =
    error "grace2: can only add a grace note to a note"


funkGroove :: Music Pitch
funkGroove =
     let p1 = perc LowTom qn
         p2 = perc AcousticSnare en
    in tempo 3 $ cut 8 $ forever
        ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
        p1 :+: p1 :+: qnr :+: p2 :+: enr)
        :=: roll en (perc ClosedHiHat 2))

allPerc = play $ line [perc s en | s <- [AcousticBassDrum .. OpenTriangle]]

scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
scaleVolume s = mMap (\(p, v) -> (p, round (s * fromIntegral v)))

fRetro :: Music a -> Music a
fRetro n@(Prim _) = n
fRetro (Modify c m) = Modify c (fRetro m)
fRetro (m1 :+: m2) = fRetro m2 :+: fRetro m1
fRetro (m1 :=: m2) =
    let d1 = dur m1
        d2 = dur m2
    in  if d1 > d2 then fRetro m1 :=: (rest (d1 - d2) :+: fRetro m2)
        else (rest (d2 - d1) :+: fRetro m1) :=: fRetro m2

fRetro2 :: Music a -> Music a
fRetro2 = mFold g (flip (:+:)) rev Modify
    where g (Note d p) = note d p
          g (Rest d) = rest d
          rev m1 m2 =
              let d1 = dur m1 
                  d2 = dur m2
              in  if d1 > d2 then m1 :=: (rest (d1 - d2) :+: m2)
                  else (rest (d2 - d1) :+: m1) :=: m2

insideOut :: Music a -> Music a 
insideOut = mFold g (:=:) (:+:) Modify 
    where g (Note d p) = note d p 
          g (Rest d) = rest d

cmajor = (c 4 wn :+: c 5 sn) :=: (rest sn :+: e 4 wn) :=:(rest en :+: g 4 wn)
tune1 = line [c 4 en, d 4 en, e 4 en, f 4 en, cmajor]


s1 = [1,5,3,6,5,0,1,1]

toIntervals :: Num a => [a] -> [[a]]
toIntervals [] = []
toIntervals ns = ns : toIntervals (diff ns)
    where diff x = zipWith (-) (tail x) x

getHeads :: [[a]] -> [a]
getHeads = map head

intervalClosures :: Num a => [a] -> [[a]]
intervalClosures = iterate intervalClosure
    where intervalClosure = getHeads . toIntervals