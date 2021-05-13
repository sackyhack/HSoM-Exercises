module Ch9 where
import Euterpea
import HSoM

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
myPMap p = defPMap p

scaleCon = defCon {
    cPlayer = defPlayer,
    cVol = 50
}

scale = line [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn, a 4 qn, b 4 qn, c 5 qn]

scaleCrescendo x = Modify (Custom "Player NewPlayer") $ Modify (Phrase [Dyn (Crescendo x)]) scale
scaleStaccato x = Modify (Phrase [Art (Staccato x)]) scale

scaleCrescendoTest x = playA myPMap scaleCon (scaleCrescendo x)