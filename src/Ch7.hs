module Ch7 where
import Euterpea

data Color = Red | Green | Blue
    deriving Show

instance Eq Color where
    Red == Red = True 
    Green == Green = True 
    Blue == Blue = True 
    _ == _ = False

instance Ord Color where
    Red <= Green = True 
    Red <= Blue = True
    Green <= Blue = True 
    a <= b = a == b

instance Enum Color where
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue

    fromEnum Red = 0
    fromEnum Green = 1
    fromEnum Blue = 2

class Temporal a where
    durT :: a -> Dur
    cutT :: Dur -> a -> a 
    removeT :: Dur -> a -> a 

instance Temporal (Music a) where
    durT = dur
    cutT = cut
    removeT = remove

instance Temporal (Primitive a) where
    durT (Note d _) = d 
    durT (Rest d) = d
    cutT d' (Note d p) = if d' < d then Note d' p else Note 0 p
    cutT d' (Rest d) = if d' < d then Rest d' else Rest 0
    removeT d' n@(Note d p) = let d'' = d - d' in cutT d'' n
    removeT d' r@(Rest d) = let d'' = d - d' in cutT d'' r

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
    f1 == f2 = map f1 [minBound..maxBound] == map f2 [minBound..maxBound]

instance Bounded Color where
    minBound = Red
    maxBound = Blue
    
colorUp Red = Green
colorUp Green = Blue
colorUp Blue = Red

colorPlus :: Color -> Color
colorPlus c = toEnum $ (fromEnum c + 1) `mod` 3

colorBlue :: Color -> Color
colorBlue _ = Blue