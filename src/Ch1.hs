module Ch1 where
import Euterpea

{-|
    Exercise 1.4 Modify the definitions of hNote and hList so that they each
    take an extra argument that specifies the interval of harmonization (rather
    than being fixed at -3). Rewrite the definition of mel to take these changes
    into account.
-}

hNote :: Dur -> Pitch -> Int -> Music Pitch
hNote d p i = note d p :=: note d (trans i p)

hList :: Dur -> [Pitch] -> Int -> Music Pitch
hList d [ ] i = rest 0
hList d (p : ps) i = hNote d p i :+: hList d ps i

p1 = (C, 4)
p2 = (G, 4)
p3 = (E,4)

mel :: Music Pitch
mel = hList qn [p1, p2, p3] (negate 3)