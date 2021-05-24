{-# LANGUAGE Arrows #-}

module Ch21 where

import Euterpea

constSF :: Clock c => a -> SigFun c a b -> SigFun c () b
constSF s sf = (arr . const) s >>> sf

foldSF :: Clock c =>
    (a -> b -> b) -> b -> [SigFun c () a] -> SigFun c () b
foldSF f b sfs =
    foldr g ((arr . const) b) sfs where
        g sfa sfb =
            proc () -> do
                s1 <- sfa -< ()
                s2 <- sfb -< ()
                outA -< f s1 s2

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [ (clarinetInstr, clarinet) ]

clarinetInstr :: InstrumentName
clarinetInstr = CustomInstrument "Clarinet"

tab3 :: Table
tab3 = tableSinesN 4096 [
    1, 0,
    0.75, 0,
    0.5, 0,
    0.14, 0,
    0.5, 0,
    0.12, 0,
    0.17, 0
    ]

clarinet :: Instr (Mono AudRate)
clarinet dur ap vol _ =
    let d = fromRational dur
    in proc () -> do
        env <- envLineSeg [0, 1, 1, 0] [d * 0.01, d * 0.98, d * 0.01] -< ()
        aud <- osc tab3 0 -< apToHz ap
        outA -< aud * env

mel :: Music1
mel =
    let m :: Music Note1
        m = Euterpea.line [na (c 4 en), na (ef 4 en), na (f 4 en),
            na (af 4 qn), na (f 4 en), na (af 4 en),
            na (bf 4 qn), na (af 4 en), na (bf 4 en),
            na (c 5 en), na (ef 5 en), na (f 5 en),
            na (af 5 wn)]
        na :: Music Pitch -> Music Note1
        na (Prim (Note d p)) = Prim (Note d (p, []))
    in instrument clarinetInstr m
    
testClarinet =
    let (dr, sf) = renderSF mel myInstrMap
    in outFile "testClarinet.wav" dr sf


bell1 :: Instr (Mono AudRate)
        -- Dur -> AbsPitch -> Volume -> AudSF () Double
bell1 dur ap vol [] =
    let f = apToHz ap
        v = fromIntegral vol/100
        d = fromRational dur
        sfs = map (\p -> (arr . const) (f * p) >>> osc tab1 0)
            [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
        in proc () -> do
            aenv <- envExponSeg [0, 1, 0.001] [0.003, d - 0.003] -< ()
            a1 <- foldSF (+) 0 sfs -< ()
            outA -< a1 * aenv * v/9

tab1 = tableSinesN 4096 [1]
bellTest1 = outFile "bell1.wav" 6 (bell1 6 (absPitch (C, 5)) 100 [])

bell2 :: Instr (Mono AudRate)
        -- Dur -> AbsPitch -> Volume -> AudSF () Double
bell2 dur ap vol [] =
    let f = apToHz ap
        v = fromIntegral vol/100
        d = fromRational dur
        sfs = map (mySF f d)
            [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
    in proc () -> do
        a1 <- foldSF (+) 0 sfs -< ()
        outA -< a1 * v/9

pow :: Floating a => a -> a -> a 
pow a b = exp (log a * b)

mySF f d p = proc () -> do
    s <- osc tab1 0 <<< (arr . const) (f * p) -< ()
    aenv <- envExponSeg [0, 1, max 0.001 (pow 0.001 (d/p))] [0.003, d/max 1 p - 0.003] -< ()
    outA -< s * aenv

bellTest2 = outFile "bell2.wav" 6 (bell2 6 (absPitch (C, 5)) 100 [])