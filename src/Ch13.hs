module Ch13 where

import Euterpea
import System.Random
import Data.List(find, groupBy, sort)


data DetGrammar a = DetGrammar a [(a, [a])]
    deriving Show 

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate g@(DetGrammar st ps) 
    | testDet g = iterate (concatMap f) [st]
    | otherwise = error "Grammar is not deterministic."
    where f a = maybe [a] id (lookup a ps)

redAlgae = DetGrammar 'a' [
    ('a', "b|c"), ('b', "b"), ('c', "b|d"),
    ('d', "e\\d"), ('e', "f"), ('f', "g"),
    ('g', "h(a)"), ('h', "h"), ('|', "|"),
    ('(', "("), (')', ")"), ('/', "\\"),
    ('\\', "/")
    ]

strToMusic :: AbsPitch -> Dur -> String -> Music Pitch 
strToMusic _ _ [] = rest 0
strToMusic a d (c:cs) = 
    case c of
        'a' -> note d (pitch a) :+: strToMusic a d cs
        'b' -> note d (pitch (a + 2)) :+: strToMusic a d cs
        'c' -> note d (pitch (a + 4)) :+: strToMusic a d cs
        'd' -> note d (pitch (a + 5)) :+: strToMusic a d cs
        'e' -> note d (pitch (a + 7)) :+: strToMusic a d cs
        'f' -> note d (pitch (a + 9)) :+: strToMusic a d cs
        'g' -> note d (pitch (a + 11)) :+: strToMusic a d cs
        'h' -> note d (pitch (a + 12)) :+: strToMusic a d cs
        '|' -> strToMusic a d cs 
        '/' -> rest d :+: strToMusic a d cs
        '\\' -> rest d :+: strToMusic a d cs 
        '(' -> strToMusic (a + 5) d cs
        ')' -> strToMusic (a - 5) d cs

try1 = play $ strToMusic 40 qn (detGenerate redAlgae !! 10)

testDet :: Eq a => DetGrammar a -> Bool 
testDet (DetGrammar start rules) =
    let ruleStarts = map fst rules
        symbols = start : ruleStarts ++ concatMap snd rules
        distinct [] = True
        distinct (x:xs) = notElem x xs && distinct xs
        allElem [] _ = True
        allElem (x:xs) ys = elem x ys && allElem xs ys
    in distinct ruleStarts && allElem symbols ruleStarts


data Grammar a = Grammar a (Rules a)
    deriving Show

data Rules a = Uni [Rule a]
               | Sto [(Rule a, Prob)]
    deriving (Eq, Ord, Show)

data Rule a = Rule { lhs :: a, rhs :: a }
    deriving (Eq, Ord, Show)

type Prob = Double

type ReplFun a = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand = Double

gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed =
    let Sto newRules = toStoRules rules
        rands = randomRs (0.0, 1.0) (mkStdGen seed)
    in if checkProbs newRules
        then generate f newRules (s, rands)
        else error "Stochastic rule-set is malformed."

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a 
toStoRules (Sto rs) = Sto rs
toStoRules (Uni rs) =
    let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
    in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)]
insertProb rules = let prb = 1.0 / fromIntegral (length rules)
                   in zip rules (repeat prb)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = all checkSum (groupBy sameLHS (sort rs))

eps = 0.001

checkSum :: [(Rule a, Prob)] -> Bool 
checkSum rules = let mySum = sum (map snd rules)
                 in abs (1.0 - mySum) <= eps

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool 
sameLHS (r1, _) (r2, _) = lhs r1 == lhs r2 

generate :: Eq a => ReplFun a -> [(Rule a, Prob)] -> (a, [Rand]) -> [a]
generate f rules xs =
    let newRules = map probDist (groupBy sameLHS rules)
        probDist rrs = let (rs, ps) = unzip rrs 
                       in zip rs (tail (scanl (+) 0 ps))
    in map fst (iterate (f newRules) xs)

data LSys a = N a
    | LSys a :+ LSys a
    | LSys a :. LSys a
    | Id
    deriving (Eq, Ord, Show)

replFun :: Eq a => ReplFun (LSys a)
replFun rules (s, rands) =
    case s of
        a :+ b -> let (a', rands') = replFun rules (a, rands)
                      (b', rands'') = replFun rules (b, rands')
                 in (a' :+ b', rands'')
        a :. b -> let (a', rands') = replFun rules (a, rands)
                      (b', rands'') = replFun rules (b, rands')
                 in (a' :. b', rands'')
        Id -> (Id, rands)
        N x -> (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand =
    let loop ((r, p) : rs) = if rand <= p then rhs r else loop rs
        loop [] = error "getNewRHS anomaly"
    in case find (\((r, p) : _) -> lhs r == ls) rrs of
        Just rs -> loop rs
        Nothing -> error "No rule match"

type IR a b = [(a, Music b -> Music b)]
interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b) r m = interpret a r (interpret b r m)
interpret (a :+ b) r m = interpret a r m :+: interpret b r m
interpret Id r m = m
interpret (N x) r m =
     case lookup x r of
        Just f -> f m
        Nothing -> error "No interpretation rule"

data LFun = Inc | Dec | Same
    deriving (Eq, Ord, Show)

ir :: IR LFun Pitch
ir = [(Inc, transpose 1),
    (Dec, transpose (-1)),
    (Same, id)]

inc, dec, same :: LSys LFun
inc = N Inc
dec = N Dec
same = N Same

sc = inc :+ dec

r1a = Rule inc (sc :. sc)
r1b = Rule inc sc
r2a = Rule dec (sc :. sc)
r2b = Rule dec sc
r3a = Rule same inc
r3b = Rule same dec
r3c = Rule same same

g1 = Grammar same (Uni [r1b, r1a, r2b, r2a, r3a, r3b])

t1 n = instrument Vibraphone $
    interpret (gen replFun g1 42 !! n) ir (c 5 tn)

try2 = play (t1 3)
try3 = play (t1 4)