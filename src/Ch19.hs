{-# LANGUAGE Arrows #-}

module Ch19 where

import Euterpea

tab1 :: Table 
tab1 = tableSinesN 4096 [1]

tab2 = tableSinesN 4096 [1.0, 0.5, 0.33]

sf :: Int -> Double -> AudSF () Double 
sf n f = proc () ->
    osc (tableSinesN n [1]) 0 -< f 

sf' :: Int -> Double -> AudSF () Double 
sf' n f = (arr . const) f >>> osc (tableSinesN n [1]) 0

testOsc name n f d = outFile (name ++ ".wav") d (sf' n f)

one = testOsc "one" 4096 440 4.0
two = testOsc "two" 1024 440 4.0
three = testOsc "three" 256 440 4.0
four = testOsc "four" 441 440 4.0
five = testOsc "five" 102 440 4.0

tremolo :: Double -> Double -> AudSF () Double 
tremolo tfrq dep = proc () -> do
    trem <- osc tab1 0 -< tfrq
    outA -< 1 + trem * dep

trem1 = proc () -> do
    trem <- tremolo 10 0.2 -<  ()
    aud <- osc tab1 0 -< 440
    outA -< aud * trem / 1.2

trem2 = proc () -> do
    trem <- tremolo 3 0.1 -<  ()
    aud <- osc tab1 0 -< 440
    outA -< aud * trem / 1.1

testTrem1 = outFile "trem1.wav" 4.0 trem1
testTrem2 = outFile "trem2.wav" 4.0 trem2

type DPair = (Double, Double)
envADSR :: DPair -> DPair -> DPair -> Double -> AudSF () Double
envADSR (ad, aa) (dd, da) (rd, ra) dur =
    envLineSeg [0, aa, da, sa, ra, 0, 0] [ad, dd, sd, rd, 0.01, 0.01]
    where sd = dur - (ad + dd + rd)
          sa = da

adsr1 = proc () -> do
    env <- envADSR (1, 1) (1, 0.3) (1, 0) 5 -<  ()
    aud <- osc tab1 0 -< 220
    outA -< aud * env

testAdsr1 = outFile "adsr1.wav" 6.0 adsr1

adsr2 = proc () -> do
    env <- envADSR (1, 1.3) (1, 0.6) (1, 0) 5 -<  ()
    aud <- osc tab1 0 -< 220
    outA -< aud * env

testAdsr2 = outFile "adsr2.wav" 6.0 adsr2

softFilter :: Clock p =>
    (Double -> Double) -> Signal p Double Double
softFilter f = proc sig -> outA -< f sig 

filterElbow d
    | abs d < 0.5 = d 
    | d > 0 = 0.5 + (d - 0.5) / 1.3
    | d < 0 = -0.5 - (d + 0.5) / 1.3

filterATan d = atan (d * 1.4) / (pi / 2)

testAdsr2Filter = outFile "adsr2f.wav" 6.0 (adsr2 >>> softFilter filterElbow)
testAdsr2Filter2 = outFile "adsr2f2.wav" 6.0 (adsr2 >>> softFilter filterATan)



mel :: Music1
mel =
    let m = Euterpea.line [na1 (c 4 en), na1 (ef 4 en), na1 (f 4 en),
            na2 (af 4 qn), na1 (f 4 en), na1 (af 4 en),
            na2 (bf 4 qn), na1 (af 4 en), na1 (bf 4 en),
            na1 (c 5 en), na1 (ef 5 en), na1 (f 5 en),
            na3 (af 5 wn)]
        na1 (Prim (Note d p)) = Prim (Note d (p, [Params [0, 0]]))
        na2 (Prim (Note d p)) = Prim (Note d (p, [Params [5, 10]]))
        na3 (Prim (Note d p)) = Prim (Note d (p, [Params [5, 20]]))
    in instrument simpleInstr m

simpleInstr :: InstrumentName
simpleInstr = CustomInstrument "Simple Instrument"

myInstr :: Instr (AudSF () Double)
myInstr dur ap vol [vfrq, dep] =
    proc () -> do
        vib <- osc tab1 0 -< vfrq
        aud <- osc tab2 0 -< apToHz ap + vib * dep
        outA -< aud

myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [
    (simpleInstr, myInstr),
    (razzerInstr, razzer),
    (wowzerInstr, wowzer)
    ]

testSimple =
    let (dr, sf) = renderSF mel myInstrMap
    in outFile "testSimple.wav" dr sf

razzerInstr :: InstrumentName
razzerInstr = CustomInstrument "Razzer"

razzer :: Instr (AudSF () Double)
razzer dur ap vol [r1, r2] =
    proc () -> do
        razz <- envLine r1 (realToFrac dur) r2 -< ()
        base <- osc tab2 0 -< apToHz ap
        outA -< base * (1 - razz) + signum base * razz

melRazzer :: Music1
melRazzer =
    let m = Euterpea.line [na1 (c 4 en), na1 (ef 4 en), na1 (f 4 en),
            na2 (af 4 qn), na1 (f 4 en), na1 (af 4 en),
            na2 (bf 4 qn), na1 (af 4 en), na1 (bf 4 en),
            na1 (c 5 en), na1 (ef 5 en), na1 (f 5 en),
            na3 (af 5 wn)]
        na1 (Prim (Note d p)) = Prim (Note d (p, [Params [0.10, 0]]))
        na2 (Prim (Note d p)) = Prim (Note d (p, [Params [0, 0.05]]))
        na3 (Prim (Note d p)) = Prim (Note d (p, [Params [0.10, 0]]))
    in instrument razzerInstr m
    
testRazzer =
    let (dr, sf) = renderSF melRazzer myInstrMap
    in outFile "testRazzer.wav" dr sf

wowzerInstr :: InstrumentName
wowzerInstr = CustomInstrument "Wowzer"

wowzer :: Instr (AudSF () Double)
wowzer dur ap vol [r1, r2, r3, r4] =
    let p = fromRational dur / 3
    in proc () -> do
        wowz <- envLineSeg [r1, r2, r3, r4] [p, p, p] -< ()
        aud <- osc tab2 0 -< apToHz ap * wowz
        outA -< aud

melWowzer :: Music1
melWowzer =
    let m = Euterpea.line [na1 (c 4 en), na1 (ef 4 en), na1 (f 4 en),
            na2 (af 4 qn), na1 (f 4 en), na1 (af 4 en),
            na2 (bf 4 qn), na1 (af 4 en), na1 (bf 4 en),
            na1 (c 5 en), na1 (ef 5 en), na1 (f 5 en),
            na3 (af 5 wn)]
        na1 (Prim (Note d p)) = Prim (Note d (p, [Params [1, 0.98, 0.98, 1]]))
        na2 (Prim (Note d p)) = Prim (Note d (p, [Params [1.02, 1.05, 1.01, 1]]))
        na3 (Prim (Note d p)) = Prim (Note d (p, [Params [0.92, 0.98, 0.99, 1]]))
    in instrument wowzerInstr m
    
testWowzer =
    let (dr, sf) = renderSF melWowzer myInstrMap
    in outFile "testWowzer.wav" dr sf