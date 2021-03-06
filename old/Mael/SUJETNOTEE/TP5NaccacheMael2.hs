
-- nom    : NACCACHE
-- prenom : Maël

-- you must define 9 functions (and answer question 10 in french or english)

-- [rec def]: a recursive definition, a function that calls itself
-- [non rec def]: a non recursive definition, a function that does not call itself 
-- [ZF def]: a function defined with a list in comprehension

-- auxiliary functions: (.), (++), (==), (/=), any, dropWhile, elem, filter, fst, head, not, snd, takeWhile, let/where and pattern matching

-- color the map of europe with 4 colors 

data Color = Blue | Red | Yellow | Green deriving (Show,Eq,Enum)

data Country =
      -- 1957
      Belgium 
    | France 
    | Italy 
    | Luxembourg  
    | Netherlands 
    | WestGermany
      -- 1973
    | Denmark
    | Ireland
    | UnitedKingdom
      -- 1981
    | Greece
      -- 1986
    | Portugal
    | Spain  
      deriving (Show,Eq,Enum)

europe57 :: [Country]
europe57 = [Belgium .. WestGermany] 
          
europe73 :: [Country]
europe73 = europe57 ++ [Denmark .. UnitedKingdom] 
          
europe81 :: [Country]
europe81 = europe73 ++ [Greece] 
          
europe86 :: [Country]
europe86 = europe81 ++ [Portugal,Spain] 
          
type Border = (Country,Country)

borders :: [Border]
borders =
    [ (Belgium,France)
    , (Belgium,Netherlands)
    , (Belgium,WestGermany)
    , (Denmark,WestGermany)
    , (France,Italy)
    , (France,Spain)
    , (France,WestGermany)
    , (Netherlands,WestGermany)
    , (Portugal,Spain)
    ]


-- PART I: GENERATE AND TEST

-- there is 4 colors

c4 :: [Color]
c4 = [Blue .. Green]

-- a list of colors is splitable when it has two or more colors

isSplitableColors :: [Color] -> Bool
isSplitableColors (_:_:_) = True
isSplitableColors _       = False

-- split a list of colors in two: one color and the others

splitColors :: [Color] -> ([Color],[Color])
splitColors (c:cs) = ([c],cs) 

-- a map associates a list of colors to a country

type Map = [(Country,[Color])]

mkInitMap :: [Country] -> Map
mkInitMap cs = zip cs (repeat c4)

-- Q1) [non rec def] a map is splitable when it contains at least one splitable list of colors

testIsSplitableMap1 :: Bool
testIsSplitableMap1 = isSplitableMap [(France,[Blue]),(Belgium,[Blue,Red,Yellow]),(Italy,[Red,Green])]
                    
testIsSplitableMap2 :: Bool
testIsSplitableMap2 = not (isSplitableMap [(France,[Blue]),(Belgium,[Blue]),(Italy,[Green])])
                    
isSplitableMap :: Map -> Bool
isSplitableMap l = any (isSplitableColors . snd) l

-- Q2) [non rec def] to split a map split the first splitable list of colors               

testSplitMap :: Bool
testSplitMap = splitMap [(France,[Blue]),(Belgium,[Blue,Red,Yellow]),(Italy,[Red,Green])] == ([(France,[Blue]),(Belgium,[Blue]),(Italy,[Red,Green])],[(France,[Blue]),(Belgium,[Red,Yellow]),(Italy,[Red,Green])])

splitMap :: Map -> (Map,Map) 
splitMap ms   = (l,r)
  where start = takeWhile (not.isSplitableColors.snd) ms
        split = dropWhile (not.isSplitableColors.snd) ms
        sh    = head (dropWhile (not.isSplitableColors.snd) ms)
        div1  = (fst sh, [head (snd sh)])
        div2  = (fst sh, tail (snd sh))
        l = start ++ [div1] ++ (tail split)
        r = start ++ [div2] ++ (tail split)

-- Q3) [rec def] generate all (unsplitable) maps

testGenerate :: Bool
testGenerate = generateMap [(France,[Blue,Yellow]),(Italy,[Red,Green])] == [[(France,[Blue]),(Italy,[Red])],[(France,[Blue]),(Italy,[Green])],[(France,[Yellow]),(Italy,[Red])],[(France,[Yellow]),(Italy,[Green])]]

generateMap :: Map -> [Map]
generateMap ms | isSplitableMap ms = generateMap m1 ++ generateMap m2
               | otherwise         = [ms]
  where (m1,m2) = splitMap ms

              
-- test map

-- Q4) [non rec def] return the list of colors of a country in a map 

testReadCountry :: Bool
testReadCountry = readCountry France [(Belgium,[Red]),(France,[Blue,Yellow]),(WestGermany,[Yellow,Green])] == [Blue,Yellow]
              
readCountry :: Country -> Map -> [Color]
readCountry p m = snd (head (dropWhile ((/=p) . fst) m))

              
-- Q5) [non rec def] test if two countries that share a border have different colors (lists of colors have size 1) 

testIsDifferentColor1 :: Bool
testIsDifferentColor1 = isDifferentColor [(Italy,[Red]),(France,[Red]),(Belgium,[Blue])] (France,Belgium)

testIsDifferentColor2 :: Bool
testIsDifferentColor2 = not (isDifferentColor [(Italy,[Red]),(France,[Red]),(Belgium,[Blue])] (France,Italy))

isDifferentColor :: Map -> Border -> Bool
isDifferentColor m b = 0 == length (filter (\p -> length (mesVoisinsDeMemeCouleur p) > 0) m)
  where
    mesVoisinsDeMemeCouleur (p1,[c1]) = filter (\(_,[c]) -> c1 == c) (mesVoisins p1)
    mesVoisins p1 = filter (\(p,_) -> estVoisin p1 p) m
    estVoisin p1 p2 = (p1,p2) == b || (p2,p1) == b

isValidMap :: [Border] -> Map -> Bool
isValidMap bs m = all (isDifferentColor m) bs

-- bruteForce (generate then test)

-- Q6) [ZF def] select only the borders corresponding to the list of countries

testFilterBorders :: Bool
testFilterBorders =  filterBorders [France,Belgium,Netherlands] borders == [(Belgium,France),(Belgium,Netherlands)] 

filterBorders :: [Country] -> [Border] -> [Border]
filterBorders ps bs = [(p1,p2) | p1 <- ps, p2 <- ps, elem (p1,p2) bs]
                 
brute :: [Country] -> [Map]
brute cs = filter (isValidMap (filterBorders cs borders)) (generateMap (mkInitMap cs))

brute57 = brute europe57 
brute73 = brute europe73 
brute81 = brute europe81 
brute86 = brute europe86 

-- PART II: GAC FILTERING FOR A MICRO CONSTRAINT SOLVER

-- an empty list of colors contains no color

isEmptyColors :: [Color] -> Bool
isEmptyColors = null

-- an empty map contains an empty list of colors

isEmptyMap :: Map -> Bool
isEmptyMap = any (isEmptyColors . snd)

-- Q7) [rec def] change the list of colors of a country in a map

testWriteCountry :: Bool
testWriteCountry = writeCountry France [Blue,Red] [(Belgium,[Red]),(France,[Blue,Red,Yellow]),(Italy,([Red]))] == [(Belgium,[Red]),(France,[Blue,Red]),(Italy,[Red])]

writeCountry :: Country -> [Color] -> Map -> Map
writeCountry p cs ((p1,c1):ms) | p == p1   = (p,cs):ms
                               | otherwise = (p1,c1):writeCountry p cs ms

-- constraint propagation (global arc consistency filtering) keep only colors of country1 that satisfy p with a color of country2
                 
filteringBorder :: Border -> Map -> Map
filteringBorder (country1,country2) m =
    let cs1 = readCountry country1 m
        cs2 = readCountry country2 m
        cs1' = filter (\c1 -> any (c1/=) cs2) cs1
        cs2' = filter (\c2 -> any (c2/=) cs1) cs2 -- Ajout Q10
        m' = writeCountry country2 cs2' m         -- Ajout Q10
    in writeCountry country1 cs1' m'

filteringAllBorders :: [Border] -> Map -> Map
filteringAllBorders bs m = foldr filteringBorder m bs

-- Q8) [rec def] compute the fixpoint of a function 

testFixPoint :: Bool
testFixPoint = fixPoint pgcd (675, 660) == (15,0)
               where pgcd :: (Int,Int) -> (Int,Int)
                     pgcd (a,0) = (a,0)
                     pgcd (a,b) = (b,a `mod` b)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint fun arg = aux fun arg (fun arg)
  where aux f a p | f p == p  = p
                  | otherwise = aux f p (f p)

globalArcConsistency :: [Border] -> Map -> Map
globalArcConsistency bs m = fixPoint (filteringAllBorders bs) m

-- Q9) [rec def] split the map into two maps, propagate constraints (apply globalArcConsistency to them), and search them

testSearch :: Bool
testSearch =  searchMap [(Belgium,France),(Belgium,WestGermany),(France,WestGermany)] [(France,[Blue]),(Belgium,c4),(WestGermany,c4)] == [[(France,[Blue]),(Belgium,[Red]),(WestGermany,[Yellow])],[(France,[Blue]),(Belgium,[Red]),(WestGermany,[Green])],[(France,[Blue]),(Belgium,[Yellow]),(WestGermany,[Red])],[(France,[Blue]),(Belgium,[Yellow]),(WestGermany,[Green])],[(France,[Blue]),(Belgium,[Green]),(WestGermany,[Red])],[(France,[Blue]),(Belgium,[Green]),(WestGermany,[Yellow])]]

searchMap :: [Border] -> Map -> [Map]
searchMap bs m | isSplitableMap m = (searchMap bs (globalArcConsistency bs l)) ++ (searchMap bs (globalArcConsistency bs r))
               | isEmptyMap m     = []
               | otherwise        = [globalArcConsistency bs m]
  where (l,r) = splitMap m 

solve :: [Country] -> [Map] 
solve cs = searchMap (filterBorders cs borders) (mkInitMap cs)
              
solve57 = solve europe57
solve73 = solve europe73
solve81 = solve europe81
solve86 = solve europe86

-- Q10) how could the solver filter more? (answer 10 words maximum) 

-- Solution a valentin : on filtre dans les deux sens.
