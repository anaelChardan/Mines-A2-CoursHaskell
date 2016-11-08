import Test.QuickCheck
import Data.Char
 
appendProp1 :: ([Bool],[Bool]) -> Bool
appendProp1 (xs,ys) = length (xs++ys) == length xs + length ys

test = quickCheck appendProp1
testV = verboseCheck appendProp1

-- random

-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_aléatoires
-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_pseudo-aléatoires
-- Générateurs congruentiels linéaires

-- a sequence of integers with a long period and a fair distribution

u :: Int -> Int
u n = (16807 * n) `mod` (2^31-1)

-- a global constant (a LAZY infinite list) 

type Random = [Int]

random :: Random
random = drop 2 (iterate u 1)

test1 = take 10 random

-- generate a random value

type Generator a = Random -> (a,Random)

rBool :: Generator Bool
--rBool :: Random -> (Bool,Random)
rBool (i:is) = (even i,is)

r3Bools :: Generator (Bool,Bool,Bool)
r3Bools random = 
    let (b1,random1) = rBool random
        (b2,random2) = rBool random1
        (b3,random3) = rBool random2
    in ((b1,b2,b3),random3)

test2 = fst (r3Bools random)

-- generate a list of bools

rListBool :: Generator [Bool]
rListBool random = 
    let (isNil,random1) = rBool random 
    in if isNil 
       then ([],random1)
       else let (head,random2) = rBool     random1
                (tail,random3) = rListBool random2
            in (head:tail,random3)

test3 = fst (rListBool random)

-- more cons than nil

rPercent :: Generator Int
rPercent (i:is) = (i `mod` 100,is)

rListBool' :: Generator [Bool]
rListBool' random = 
    let (isNil,random1) = rPercent random 
    in if (isNil<20) 
       then ([],random1)
       else let (head,random2) = rBool      random1
                (tail,random3) = rListBool' random2
            in (head:tail,random3)

test3' = fst (rListBool' random)

-- generate other type of elements

rChar :: Generator Char
rChar (i:is) = (chr (ord 'a' + i `mod` 26),is)

rList :: Generator a -> Generator [a]
rList genElt random = 
    let (isNil,random1) = rPercent random 
    in if (isNil<20) 
       then ([],random1)
       else let (head,random2) = genElt       random1
                (tail,random3) = rList genElt random2
            in (head:tail,random3)

test4 = fst (rList rChar random)

test3'' = fst (rList rBool random)


-- a quickcheck like

class TestGenerator a where
    generate :: Generator a -- rappel : Random -> (a,Random)

instance TestGenerator Int where
    generate (r:rs) = (r,rs)

instance TestGenerator Bool where
    generate (r:rs) = (even r,rs)

instance TestGenerator Char where
    generate (r:rs) = (chr (ord 'a' + r `mod` 26),rs)

instance TestGenerator a => TestGenerator [a] where
    generate random = 
        let (isNil,random1) = rPercent random 
        in if (isNil<30)
           then ([],random1)
           else let (head,random2) = generate random1 -- generate is called here
                    (tail,random3) = generate random2
                in (head:tail,random3)

-- yet another type
instance (TestGenerator a, TestGenerator b) => TestGenerator (a,b) where
    generate random = 
        let (a,random1) = generate random
            (b,random2) = generate random1
        in ((a,b),random2)

-- type base selection
--testB :: Int

--testB :: [Int]
--testB :: [Bool]
--testB :: [[Bool]]
--testB :: [(Int,[Bool])]
testB :: [(Int,[(Bool,String)])]
testB = fst (generate random)

appendProp1' :: ([Int],[Int]) -> Bool
appendProp1' (xs,ys) = length (xs++ys) == length xs + length ys 

myQuickCheck prop = all prop (fst (generate random))

myTest = myQuickCheck appendProp1'


-- MONAD

-- bug alert: beware to thread the state correctly

rListBug' :: Generator a -> Generator [a]
rListBug' genElt gen = 
    let (isNil,gen1) = rPercent gen 
    in if (isNil<30)
       then ([],gen1)
       else let (head,gen2) = genElt           gen1
                (tail,gen3) = rListBug' genElt gen
            in (head:tail,gen3)

test6 = fst (rListBug' rChar random)


-- random monadic

rReturnM :: a -> Generator a
rReturnM a random = (a,random)

rThenM :: Generator a -> (a -> Generator b) -> Generator b
rThenM f g random = let (r,random1) = f random
                    in g r random1

rListM :: Generator a -> Generator [a]
rListM genElt = 
    rPercent                  `rThenM` (\isNil -> 
    if (isNil<20)
    then rReturnM []
    else genElt               `rThenM` (\head -> 
         rListM genElt        `rThenM` (\tail ->
         rReturnM (head:tail))))

test7 = fst (rListM rChar random)
