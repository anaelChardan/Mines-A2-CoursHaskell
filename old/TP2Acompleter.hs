
-- on generalise (autant que possible) le type des fonctions du bloc1

myHead :: [a] -> a
myHead (x:_) = x
               
myTail :: [a] -> [a]
myTail (_:xs) = xs

--c1 :: [a]
--c1 = []
                
myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs 
    where -- myAppend' :: [b] -> [b]
          myAppend' (x:xs) = x:myAppend' xs
          myAppend' [] = ys  

myInit :: [b] -> [b]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

myLast :: [c] -> c
myLast [x] = x
myLast (_:xs) = myLast xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse xs = xs

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat [] = []

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n [] = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:xs) 0 = x
myBangBang (x:xs) n = myBangBang xs (n-1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x>y       = y:myInsert x ys
                  | otherwise = x:y:ys

mySort :: Ord a => [a] -> [a]
mySort (x:xs) = myInsert x (mySort xs)
mySort [] = []

-- NEW STUFF

-- ordre superieur

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) | f x       = x:myTakeWhile f xs
                     | otherwise = []
myTakeWhile f []                 = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose :: (b->c) -> (a->b) -> a -> c
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x:myMap f xs
myMap f []     = []

test1 = myMap odd [1..10]

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x:xs) = sL ++ (map (x:) sL)
    where sL = sousListes xs
sousListes [] = [[]]
                
-- une fonction plus generale: foldr
-- inferer le type de foldr
-- forme graphique de la liste en peigne
--myFoldr :: 
--myFoldr f k (x:xs) = f x (myFoldr f k xs)
--myFoldr f k []     = k

myAnd' :: [Bool] -> Bool
--myAnd' [] = True
--myAnd' bs = foldr (&&) True bs
myAnd' = foldr (&&) True

-- eta reduction
         
-- definir reverse avec foldr
myReverse' :: [a] -> [a]
myReverse' = undefined

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' x y = x+y
           
add'' :: Int -> Int -> Int
add'' = \x y -> x+y

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' = undefined

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = undefined

-- un "nouveau type" String

-- String
s1 :: String
s1 = "azertyuiop"

-- un nouveau type tuples

myFst :: (a,b) -> a
myFst (x,_) = x

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs) | f x = myDropWhile f xs
                     | otherwise  = x:xs
myDropWhile  f []     = []

myElem :: Eq a => a -> [a] -> Bool
myElem a (x:xs) = a == x || myElem a xs
myElem a []     = False

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem a k = not (myElem a k)
--myNotElem a (x:xs)     = not (e == x) && myNotElem e xs
--myNotElem e []         = True

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond (x:xs) | cond x      = x : myFilter cond xs
                     | otherwise   = myFilter cond xs
myFilter cond []     = []

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt 0 xs     = ([], xs)
mySplitAt n (x:xs) = (x:xs', xs'')
  where (xs', xs'') = mySplitAt (n - 1) xs

myZip :: [a] -> [b] -> [(a,b)] 
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (x,y) = f x y

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' = undefined

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip = undefined

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' x = foldr (++) [] x

myMap' ::  (a -> b) -> [a] -> [b]
myMap' f y = foldr (\x acc -> f x : acc) [] y

myOr' ::  [Bool] -> Bool
myOr' y = foldr (||) False y

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x res -> f x || res) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll x = foldr (\x res -> x && res) True x

myProduct :: [Int] -> Int
myProduct x = foldr (*) 1 x

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

premiers :: [Int]
premiers = undefined
premiers = filter (\x -> [2..x])[2..]
premier acc (x:xs)= premier (x:acc) (filter (\y-> y `mod` x/= 0) xs)
premier l []=l

test2 = take 50 premiers

