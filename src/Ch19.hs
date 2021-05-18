{-# LANGUAGE Arrows #-}

module Ch19 where

import Euterpea

tab1 :: Table 
tab1 = tableSinesN 4096 [1]

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
