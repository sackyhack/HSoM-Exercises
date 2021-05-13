module Ch3 where
import Euterpea

f1 :: Int -> [Pitch] -> [Pitch]
f1 i = map (trans i)

f2 :: [Dur] -> [Music a]
f2 = map rest

f3 :: [Music Pitch] -> [Music Pitch]
f3 = let staccato (Prim(Note d p)) = note (d / 2) p :+: rest (d / 2)
     in map staccato

applyAll :: [a -> a] -> a -> a
applyAll [] v = v
applyAll (f:fs) v = f (applyAll fs v)

applyAll2 fs v = let apply f = f
                 in foldr apply v fs

test = applyAll [(* 4), (+ 3)] 5
test2 = applyAll2 [(* 4), (+ 3)] 5

length :: [a] -> Int 
length = let incr i _ = i + 1
         in foldl incr 0

doubleEach = map (* 2)
pairAndOne = map (\x -> (x, x + 1))
addEachPair = map (\(x, y) -> x + y)
addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)
addPairsPointwise = foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse [] [] = []
fuse [] (p:ps) = error "not enough durations"
fuse (d:ds) [] = error "not enough pitches"
fuse (d:ds) (p:ps) = p d : fuse ds ps

maxAbsPitch :: [Pitch] -> Pitch 
maxAbsPitch [] = error "no max of empty list"
maxAbsPitch [p] = p
maxAbsPitch (p:ps) = let maxPs = maxAbsPitch ps in if p > maxPs then p else maxPs

maxAbsPitch' :: [Pitch] -> Pitch 
maxAbsPitch' = let max p1 p2 = if p1 > p2 then p1 else p2 
               in foldl1 max

minAbsPitch :: [Pitch] -> Pitch 
minAbsPitch [] = error "no min of empty list"
minAbsPitch [p] = p
minAbsPitch (p:ps) = let minPs = minAbsPitch ps in if p < minPs then p else minPs

minAbsPitch' :: [Pitch] -> Pitch 
minAbsPitch' = let min p1 p2 = if p1 < p2 then p1 else p2 
               in foldl1 min

chrom :: Pitch -> Pitch -> Music Pitch 
chrom p1 p2
    | p1 == p2 = note qn p1
    | p1 > p2 =  note qn p1 :+: chrom (trans (negate 1) p1) p2 
    | otherwise = note qn p1 :+: chrom (trans 1 p1) p2

chrom' :: Pitch -> Pitch -> Music Pitch 
chrom' p1 p2 =
    foldl (:+:) (rest 0) (map (note qn . pitch) [(absPitch p1) .. (absPitch p2)])

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p is = line (map (\i -> note qn (trans i p)) (scanl (+) 0 is))

data MajorScaleMode = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian

genScale :: Pitch -> MajorScaleMode -> Music Pitch
genScale p mode = 
  let intervals  = cycle [2, 2, 1, 2, 2, 2, 1]
      getIntervals n = take 7 $ drop n $ intervals
      getModeIntervals Ch3.Ionian = getIntervals 0
      getModeIntervals Ch3.Dorian = getIntervals 1
      getModeIntervals Ch3.Phrygian = getIntervals 2
      getModeIntervals Ch3.Lydian = getIntervals 3
      getModeIntervals Ch3.Mixolydian = getIntervals 4
      getModeIntervals Ch3.Aeolian = getIntervals 5
      getModeIntervals Ch3.Locrian = getIntervals 6
  in mkScale p (getModeIntervals mode)