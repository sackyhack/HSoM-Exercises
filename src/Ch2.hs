module Ch2 where
import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch 
twoFiveOne p d =
    let two  = note d (trans 2 p) :=: note d (trans 5 p) :=: note d (trans 9 p)
        five = note d (trans 7 p) :=: note d (trans 11 p) :=: note d (trans 14 p)
        one  = note (d * 2) p :=: note (d * 2) (trans 4 p) :=: note (d * 2) (trans 7 p)
    in two :+: five :+: one


data BluesPitchClass = Ro | MT | Fo | Fi | MS

type BluesPitch = (BluesPitchClass, Octave)

ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch 
fromBlues (Prim (Note d (Ro, o))) = Prim (Note d (C, o)) 
fromBlues (Prim (Note d (MT, o))) = Prim (Note d (Ef, o)) 
fromBlues (Prim (Note d (Fo, o))) = Prim (Note d (F, o)) 
fromBlues (Prim (Note d (Fi, o))) = Prim (Note d (G, o)) 
fromBlues (Prim (Note d (MS, o))) = Prim (Note d (Bf, o)) 
fromBlues (Prim (Rest d)) = Prim (Rest d) 
fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2 
fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues (Modify ctrl m) = Modify ctrl (fromBlues m)

melody1 = fromBlues( fi 4 dqn :+: ms 4 en :+: fo 4 qn :+: mt 4 hn :+: ro 4 qn :+: mt 4 hn )
melody2 = fromBlues ( fi 3 dhn :+: mt 3 dhn :+: fi 3 dhn :+: rest hn )
melody3 = fromBlues( fi 5 qn :+: rest en :+: times 2 ( fi 5 sn ) :+:
                     fi 5 en :+: rest sn :+: fi 5 en :+: rest sn :+:
                     times 2 (fi 5 qn ) :+: fi 5 en )
melody4 = fromBlues( mt 6 qn :+: rest en :+: times 2 ( mt 6 sn ) :+:
                     mt 5 en :+: rest sn :+: mt 6 en :+: rest sn :+:
                     times 2 (mt 6 qn ) :+: mt 6 en )
                     
bluesSong = times 2 ( melody1 :=: melody2 :=: melody3 :=: melody4) 

transM :: AbsPitch -> Music Pitch -> Music Pitch 
transM ap (Prim (Note d p)) = Prim (Note d (trans ap p)) 
transM ap (Prim (Rest d)) = Prim (Rest d) 
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2 
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify ctrl m) = Modify ctrl (transM ap m)