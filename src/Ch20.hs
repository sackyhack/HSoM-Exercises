module Ch20 where

import Data.Complex

dft :: RealFloat a => [Complex a] -> [Complex a]
dft xs =
    let lenI = length xs
        lenR = fromIntegral lenI
        lenC = lenR :+ 0
    in [let i = negate 2 * pi * fromIntegral k/lenR
        in (1/lenC) * sum [(xs !! n) * exp (0 :+ i * fromIntegral n)
            | n <- [0, 1 .. lenI - 1]]
        | k <- [0, 1 .. lenI - 1]]

-- This seems to work, but I don't truly understand it
idft :: RealFloat a => [Complex a] -> [Complex a]
idft xs =
    let lenI = length xs
        lenR = fromIntegral lenI
        lenC = lenR :+ 0
    in [let i = 2 * pi * fromIntegral k/lenR
        in sum [(xs !! n) * exp (0 :+ i * fromIntegral n)
            | n <- [0, 1 .. lenI - 1]]
        | k <- [0, 1 .. lenI - 1]]

printComplexL :: [Complex Double] -> IO ()
printComplexL xs =
    let f (i, rl :+ im) =
            do putStr (spaces (3 - length (show i)))
               putStr (show i ++ ": (" )
               putStr (niceNum rl ++ ", " )
               putStr (niceNum im ++ ")\n" )
    in mapM_ f (zip [0 .. length xs - 1] xs)

niceNum :: Double -> String
niceNum d =
    let d' = fromIntegral (round (1e10 * d))/1e10
        (dec, fra) = break (== '.') (show d')
        (fra', exp) = break (== 'e') fra
    in spaces (3 - length dec) ++ dec ++ take 11 fra'
        ++ exp ++ spaces (12 - length fra' - length exp)

spaces :: Int -> String
spaces n = take n (repeat ' ')

mkTerm :: Int -> Double -> [Complex Double]
mkTerm num n = let f = 2 * pi/fromIntegral num
    in [sin (n * f * fromIntegral i)/n :+ 0
        | i <- [0, 1 .. num - 1]]

mkxa, mkxb, mkxc :: Int -> [Complex Double]
mkxa num = mkTerm num 1
mkxb num = zipWith (+) (mkxa num) (mkTerm num 3)
mkxc num = zipWith (+) (mkxb num) (mkTerm num 5)

testXc n = printComplexL (dft (mkxc n))

mkPulse :: Int -> [Complex Double]
mkPulse n = 100 : take (n - 1) (repeat 0)

testPulse n = printComplexL (dft (mkPulse n))