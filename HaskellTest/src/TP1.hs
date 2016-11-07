import           Data.List

-- un commentaire sur une ligne
{-
imbriques

{-
commentaire
sur
plusieurs
lignes
-}
-}
-- une constante est une constante
-- on peut pas changer sa valeur
un :: Int
un = 1

deux :: Int
deux = 2

trois :: Int
trois = deux + un

c4 :: Int
c4 = 1 - 3

c5 :: Int
c5 = (-) 1 3

--mySub :: Int -> Int -> Int
mySub :: Int -> (Int -> Int)
mySub x y = x - y

neg :: Int -> Int
neg = mySub 0

quatre :: Int
--quatre = mySub 5 1
quatre = 5 `mySub` 1

b1 :: Bool
b1 = True

b2 :: Bool
b2 = not (True && False)

b3 :: Bool
b3 = 1 == 2

b4 :: Bool
b4 = 1 /= 2

l1 :: [Int]
l1 = []

l2 :: [Int]
l2 = 1 : l1 -- (:) :: Int -> [Int] -> [Int]

l3 :: [Int]
l3 = 2 : l2

l4 :: [Int]
l4 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

l5 :: [Int]
l5 = [1 .. 10]

l6 :: [Int]
l6 = [1,4 .. 11]

l7 :: [Int]
l7 = [10,8 .. (-12)]

myHead :: [Int] -> Int
myHead (x:xs) = x

deuxiemeElement :: [Int] -> Int
--deuxiemeElement xs = myHead (myTail xs)
deuxiemeElement (_:x2:_) = x2

myTail :: [Int] -> [Int]
myTail (_:xs) = xs

lA :: [Int]
lA = [1 .. 5]

lB :: [Int]
lB = [6 .. 10]

lZ1 :: [Int]
lZ1 = lA ++ lB

lA' :: [Int]
lA' = [2 .. 5]

lZ2 :: [Int]
lZ2 = lA' ++ lB

append :: [Int] -> [Int] -> [Int]
append (x:xs) ys = x : append xs ys
append [] ys     = ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys
  | not (null xs) = head xs : myAppend' (tail xs) ys
  | otherwise = ys

myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' xs ys
  | null xs = ys
  | not (null xs) = head xs : myAppend'' (tail xs) ys

myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x:xs) ys =
  let suite = myAppend4 xs ys
  in x : suite
myAppend4 [] ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 (x:xs) ys = x : suite
  where
    suite = myAppend5 xs ys
myAppend5 [] ys = ys

myAppend6 :: [Int] -> [Int] -> [Int]
myAppend6 xs ys = myAppend6' xs
  where
    myAppend6' (x:xs) = x : myAppend6' xs
    myAppend6' []     = ys

-------------------------
myInit :: [Int] -> [Int]
myInit [x]    = []
myInit (x:xs) = x : myInit xs

myLast :: [Int] -> Int
myLast [x]    = x
myLast (_:xs) = myLast xs

myNull :: [Int] -> Bool
myNull []    = True
myNull (_:_) = False

myNull' :: [Int] -> Bool
myNull' (x:xs) = length xs == 0

myLength :: [Int] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [Int] -> [Int]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' xs = myReverse'' xs []
  where
    myReverse'' (x:xs) rs = myReverse'' rs (x : rs)
    myReverse'' [] rs     = rs

myConcat :: [[Int]] -> [Int]
myConcat []     = []
myConcat (x:xs) = x ++ myConcat xs

myConcat' :: [[Int]] -> [Int]
myConcat' = foldr (++) []

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myProduct :: [Int] -> Int
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

-- pas d'element neutre pour max et min !
myTake :: Int -> [Int] -> [Int]
myTake 0 _      = []
myTake i (x:xs) = x : myTake (i - 1) xs

myDrop :: Int -> [Int] -> [Int]
myDrop 0 xs     = xs
myDrop _ []     = []
myDrop i (_:xs) = myDrop (i - 1) xs

myBangBang :: [Int] -> Int -> Int
myBangBang (x:xs) 0 = x
myBangBang (x:xs) i = myBangBang xs (i - 1)

myInsert :: Int -> [Int] -> [Int]
myInsert n (x:xs)
  | n <= x = n : x : xs
  | otherwise = x : myInsert n xs
myInsert n [] = [n]

mySort :: [Int] -> [Int]
mySort []     = []
mySort (x:xs) = myInsert x (mySort xs)
