module Ch9 where
import Euterpea
import HSoM

import Data.List(sort)

newPlayer :: Player (Pitch, [NoteAttribute])
newPlayer = defPlayer {
    pName = "NewPlayer",
    interpPhrase = defInterpPhrase myPasHandler
}

myPasHandler :: PhraseAttribute -> Performance -> Performance
myPasHandler (Dyn (Crescendo x)) pf = 
    let tmax = maximum $ map eTime pf
        t0 = eTime (head pf)
        dur = tmax - t0
        r = x / dur
        upd e@MEvent {eTime = t, eVol = v} =
            e {eVol = round ((1 + (t - t0) * r) * fromIntegral v)}
    in map upd pf
myPasHandler pa pf = defPasHandler pa pf

myPMap :: PlayerName -> Player Note1 
myPMap "NewPlayer" = newPlayer
myPMap "MyPlayer" = myPlayer
myPMap p = defPMap p

scaleCon = defCon {
    cPlayer = defPlayer,
    cVol = 50
}

scale = line [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn, a 4 qn, b 4 qn, c 5 qn]

scaleCrescendo x = Modify (Custom "Player NewPlayer") $ Modify (Phrase [Dyn (Crescendo x)]) scale
scaleStaccato x = Modify (Phrase [Art (Staccato x)]) scale

scaleCrescendoTest x = playA myPMap scaleCon (scaleCrescendo x)

myPlayer = defPlayer {
    pName = "MyPlayer",
    interpPhrase = myInterpPhrase
}

myInterpPhrase :: PhraseFun a 
myInterpPhrase pm c [] m = perf pm c m
myInterpPhrase pm 
    c@Context { cTime = t, cPlayer = pl, cInst = i,
        cDur = dt, cPch = k, cVol = v }
    (pa:pas) m =
        let pfd@(pf, dur) = myInterpPhrase pm c pas m
            first = head pf
            arpDurRatio = 32
        in case pa of 
            Art Pedal -> -- Extend duration of each event to the end of the phrase
                let t0 = eTime (head pf)
                    upd e@MEvent {eTime = t} = e {eDur = dur - (t - t0)} 
                in (map upd pf, dur)
            Orn ArpeggioUp ->
                let (arp, rest) = span (\e -> eTime e == eTime first && eInst e == eInst first) pf
                    arp' = map (\(n, e@MEvent {eTime = t, eDur = d}) ->
                         e {eTime = t + dt * n / arpDurRatio, eDur = d - dt * n / arpDurRatio}) $ zip [0..] (sort arp)
                in (merge arp' rest, dur)
            Orn ArpeggioDown -> 
                let (arp, rest) = span (\e -> eTime e == eTime first && eInst e == eInst first) pf
                    arp' = map (\(n, e@MEvent {eTime = t, eDur = d}) ->
                         e {eTime = t + dt * n / arpDurRatio, eDur = d - dt * n / arpDurRatio}) $ zip [0..] (reverse (sort arp))
                in (merge arp' rest, dur)
            _ -> pfd

pedalTest = playA myPMap scaleCon $ Modify (Custom "Player MyPlayer") $ Modify (Phrase [Art Pedal]) scale

arpeggio = (c 4 qn :=: e 4 qn :=: g 4 qn :=: c 5 qn) :+: d 4 qn :+: e 4 qn :+: f 4 qn :+: g 4 qn
arpUpTest = playA myPMap scaleCon $ Modify (Custom "Player MyPlayer") $ Modify (Phrase [Orn ArpeggioUp]) arpeggio
arpDownTest = playA myPMap scaleCon $ Modify (Custom "Player MyPlayer") $ Modify (Phrase [Orn ArpeggioDown]) arpeggio
