{-# LANGUAGE Arrows #-}

module Ch23 where

import Euterpea
import Euterpea.IO.Audio.CSound(delay)

-- instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
--    delay i = ArrowP (delay i)

sineTable441 :: Table
sineTable441 = tableSinesN 100 [1]

s441 :: AudSF () Double
s441 = proc () -> do
    rec s <- delayLineT 100 sineTable441 -< s
    outA -< s

ts441 = outFile "s441.wav" 5 s441

echo :: AudSF Double Double
echo = proc s -> do
    rec fb <- delayLine 0.5 -< s + 0.7 * fb
    outA -< fb/3

modVib :: Double -> Double -> AudSF Double Double
modVib rate depth =
    proc sin -> do
        vib <- osc sineTable441 0 -< rate
        sout <- delayLine1 0.2 -< (sin, 0.1 + 0.005 * vib)
        outA -< sout

tModVib = outFile "modvib.wav" 6 $
    (arr . const) 440 >>> osc sineTable441 0 >>> modVib 5 0.005

flute :: Dur -> Double -> Double -> Double -> Double
    -> AudSF () Double
flute dur amp fqc press breath =
    let d = fromRational dur
    in proc () -> do
        env1 <- envLineSeg [0, 1.1 * press, press, press, 0]
                          [0.06, 0.2, d - 0.16, 0.02] -< ()
        env2 <- envLineSeg [0, 1, 1, 0]
                          [0.01, d - 0.02, 0.01] -< ()
        envib <- envLineSeg [0, 0, 1, 1]
                          [0.5, 0.5, d - 1] -< ()
        flow <- noiseWhite 42 -< ()
        vib <- osc sineTable 0 -< 5
        let emb = breath * flow * env1 + env1 + vib * 0.1 * envib
        rec flute <- delayLine (1/fqc) -< out
            x <- delayLine (1/fqc/2) -< emb + flute * 0.4
            out <- filterLowPassBW -< (x - x * x * x + flute * 0.4, 2000)
        outA -< out * amp * env2

sineTable :: Table
sineTable = tableSinesN 4096 [1]

tFlute = outFile "tFlute.wav" 5 $ flute 5 0.7 440 0.99 0.2

f0 = flute 3 0.35 440 0.93 0.02 -- average breath
f1 = flute 3 0.35 440 0.83 0.05 -- weak breath, soft note
f2 = flute 3 0.35 440 0.53 0.04 -- very weak breath, no note

dcBlock :: Double -> AudSF Double Double
dcBlock a = proc xn -> do
    rec let yn = xn - xn_1 + a * yn_1
        xn_1 <- delay 0 -< xn
        yn_1 <- delay 0 -< yn
    outA -< yn

waveguide :: Double -> Double -> Double ->
    AudSF (Double, Double) (Double, Double)
waveguide del ga gb = proc (ain, bin) -> do
    rec bout <- delayLine del -< bin - ga * aout
        aout <- delayLine del -< ain - gb * bout
    outA -< (aout, bout)