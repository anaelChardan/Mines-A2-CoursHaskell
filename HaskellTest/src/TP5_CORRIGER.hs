
-- nom    : Lassont
-- prenom : Florian

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
isSplitableMap = any (isSplitableColors . snd)


-- Q2) [non rec def] to split a map split the first splitable list of colors               

testSplitMap :: Bool
testSplitMap = splitMap [(France,[Blue]),(Belgium,[Blue,Red,Yellow]),(Italy,[Red,Green])] == ([(France,[Blue]),(Belgium,[Blue]),(Italy,[Red,Green])],[(France,[Blue]),(Belgium,[Red,Yellow]),(Italy,[Red,Green])])

splitMap :: Map -> (Map,Map) 
splitMap m = (h ++ (c,cs1) : t, h ++ (c,cs2) : t)
             where
               h         = takeWhile (not . isSplitableColors . snd) m
               (c,cs):t  = dropWhile (not . isSplitableColors . snd) m
               (cs1,cs2) = splitColors cs


-- Q3) [rec def] generate all (unsplitable) maps

testGenerate :: Bool
testGenerate = generateMap [(France,[Blue,Yellow]),(Italy,[Red,Green])] == [[(France,[Blue]),(Italy,[Red])],[(France,[Blue]),(Italy,[Green])],[(France,[Yellow]),(Italy,[Red])],[(France,[Yellow]),(Italy,[Green])]]

generateMap :: Map -> [Map]
generateMap m | isSplitableMap m = let (m1,m2) = splitMap m in generateMap m1 ++ generateMap m2
              | otherwise        = [m]

              
-- test map

-- Q4) [non rec def] return the list of colors of a country in a map 

testReadCountry :: Bool
testReadCountry = readCountry France [(Belgium,[Red]),(France,[Blue,Yellow]),(WestGermany,[Yellow,Green])] == [Blue,Yellow]
              
readCountry :: Country -> Map -> [Color]
readCountry c m = (snd . head) (filter ((== c) . fst) m)

              
-- Q5) [non rec def] test if two countries that share a border have different colors (lists of colors have size 1) 

testIsDifferentColor1 :: Bool
testIsDifferentColor1 = isDifferentColor [(Italy,[Red]),(France,[Red]),(Belgium,[Blue])] (France,Belgium)

testIsDifferentColor2 :: Bool
testIsDifferentColor2 = not (isDifferentColor [(Italy,[Red]),(France,[Red]),(Belgium,[Blue])] (France,Italy))

isDifferentColor :: Map -> Border -> Bool
isDifferentColor m (c1,c2) = (readCountry c1 m) /= (readCountry c2 m)

isValidMap :: [Border] -> Map -> Bool
isValidMap bs m = all (isDifferentColor m) bs

-- bruteForce (generate then test)

-- Q6) [ZF def] select only the borders corresponding to the list of countries

testFilterBorders :: Bool
testFilterBorders =  filterBorders [France,Belgium,Netherlands] borders == [(Belgium,France),(Belgium,Netherlands)] 

filterBorders :: [Country] -> [Border] -> [Border]
filterBorders cs b = [bs | bs <- b, fst bs `elem` cs, snd bs `elem` cs]
                 
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
writeCountry country colors []      = []
writeCountry country colors (c:cs) | fst c == country = (fst c, colors) : cs
                                   | otherwise        = c : writeCountry country colors cs

-- constraint propagation (global arc consistency filtering) keep only colors of country1 that satisfy p with a color of country2
                 
filteringBorder :: Border -> Map -> Map
filteringBorder (country1,country2) m =
    let cs1 = readCountry country1 m
        cs2 = readCountry country2 m
        cs1' = filter (\c1 -> any (c1/=) cs2) cs1
    in writeCountry country1 cs1' m

filteringAllBorders :: [Border] -> Map -> Map
filteringAllBorders bs m = foldr filteringBorder m bs

-- Q8) [rec def] compute the fixpoint of a function 

testFixPoint :: Bool
testFixPoint = fixPoint pgcd (675, 660) == (15,0)
               where pgcd :: (Int,Int) -> (Int,Int)
                     pgcd (a,0) = (a,0)
                     pgcd (a,b) = (b,a `mod` b)

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f a
         | a == f a  = a
         | otherwise = fixPoint f (f a)

globalArcConsistency :: [Border] -> Map -> Map
globalArcConsistency bs m = fixPoint (filteringAllBorders bs) m

-- Q9) [rec def] split the map into two maps, propagate constraints (apply globalArcConsistency to them), and search them

testSearch :: Bool
testSearch =  searchMap [(Belgium,France),(Belgium,WestGermany),(France,WestGermany)] [(France,[Blue]),(Belgium,c4),(WestGermany,c4)] == [[(France,[Blue]),(Belgium,[Red]),(WestGermany,[Yellow])],[(France,[Blue]),(Belgium,[Red]),(WestGermany,[Green])],[(France,[Blue]),(Belgium,[Yellow]),(WestGermany,[Red])],[(France,[Blue]),(Belgium,[Yellow]),(WestGermany,[Green])],[(France,[Blue]),(Belgium,[Green]),(WestGermany,[Red])],[(France,[Blue]),(Belgium,[Green]),(WestGermany,[Yellow])]]

searchMap :: [Border] -> Map -> [Map]
searchMap bs m
          | isSplitableMap m = let (m1,m2) = splitMap m in filter (not . isEmptyMap) (searchMap bs (globalArcConsistency bs m1) ++ searchMap bs (globalArcConsistency bs m2))
          | otherwise        = [m]

solve :: [Country] -> [Map] 
solve cs = searchMap (filterBorders cs borders) (mkInitMap cs)
              
solve57 = solve europe57
solve73 = solve europe73
solve81 = solve europe81
solve86 = solve europe86

-- Q10) how could the solver filter more? (answer 10 words maximum) 
-- Removing combinations where colors are only shifted between countries
