module Ch5 where
import Euterpea

twice :: (a -> a) -> (a -> a)
twice f = f . f

power :: (a -> a) -> Int -> (a -> a)
power f 1 = f 
power f n = f . power f (n - 1)

fix f = f (fix f)

remainder :: Integer -> Integer -> Integer 
remainder = fix (\g a b -> if a < b then a else g (a - b) b)

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs as bs = [(a, b) | a <- as, b <- bs, a - b > 2, a - b < 8]

musicPairs :: [(AbsPitch, AbsPitch)] -> Music Pitch
musicPairs ps = line [note d (pitch a) :=: note d (pitch b) | (a, b) <- ps, d <- [en, sn], (odd (a - b) && d == en) || (even (a - b) && d == sn)]

hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (negate 3) p)

hList d = line . map (hNote d)

hList2 = \d -> line . map (hNote d)

addDur :: Dur -> [Dur -> Music a] -> Music a 
addDur d ns = line [n d | n <- ns]

addDur2 d ns = line $ map (\f -> f d) ns

fun57 = map ((/2) . (+1))

fun58 f g = map (f . g)

fun57b = map (/2) . map (+1)
