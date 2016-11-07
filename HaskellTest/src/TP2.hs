-- on generalise (autant que possible) le type des fonctions du bloc1
myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

--c1 :: [a]
--c1 = []
myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs
  where
    myAppend' (x:xs) = x : myAppend' xs
    myAppend' []     = ys

-- myAppend' :: [b] -> [b]
myInit :: [b] -> [b]
myInit [_]    = []
myInit (x:xs) = x : (myInit xs)

myLast :: [c] -> c
myLast [x]    = x
myLast (_:xs) = myLast xs

myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse xs     = xs

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat []       = []

myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake n []     = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop n []     = []
myDrop n (x:xs) = myDrop (n - 1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:xs) 0 = x
myBangBang (x:xs) n = myBangBang xs (n - 1)

myInsert
  :: Ord a
  => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys)
  | x > y = y : myInsert x ys
  | otherwise = x : y : ys

mySort
  :: Ord a
  => [a] -> [a]
mySort (x:xs) = myInsert x (mySort xs)
mySort []     = []

-- NEW STUFF
-- ordre superieur
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
  | f x = x : myTakeWhile f xs
  | otherwise = []
myTakeWhile f [] = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose :: (b -> c) -> (a -> b) -> a -> c
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap f []     = []

test1 = myMap odd [1 .. 10]

-- calcul des sous liste en utilisant map
sousListes :: [a] -> [[a]]
sousListes (x:xs) = sL ++ (map (x :) sL)
  where
    sL = sousListes xs
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
myReverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- une parenthese sur les lambda anonymes
add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> Int -> Int
add'' = \x y -> x + y

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = foldr (\x acc -> acc ++ [x]) []

-- un "nouveau type" String
-- String
s1 :: String
s1 = "azertyuiop"

-- un nouveau type tuples
myFst :: (a, b) -> a
myFst (x, _) = x

-- TODO: definir recursivement
-- myDropWhile (<5) [1..10]
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs)
  | f x = myDropWhile f xs
  | otherwise = x : myDropWhile f xs
myDropWhile f [] = []

myElem
  :: Eq a
  => a -> [a] -> Bool
myElem a (x:xs) = a == x || myElem a xs
myElem _ []     = False

myNotElem
  :: Eq a
  => a -> [a] -> Bool
myNotElem a xs = not (myElem a xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs = ([], xs)
mySplitAt i (x:xs) = (x : xs', xs'')
  where
    (xs', xs'') = mySplitAt (i - 1) xs

myZip :: [a] -> [b] -> [(a, b)]
myZip = undefined

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith = undefined

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x,y) = f x y

-- TODO : Faire ces deux fonctions
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' = undefined

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = undefined

-- DONE
myConcat' :: [[a]] -> [a]
myConcat' = foldr (++) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x acc -> f x : acc) []

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\x acc -> f x && acc) True

myProduct :: [Int] -> Int
myProduct = foldr (*) 1

premiers :: [Int]
premiers = undefined

test2 = take 50 premiers
