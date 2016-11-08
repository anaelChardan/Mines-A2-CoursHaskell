
--import Data.List

{-
-- commentaire sur une ligne

{- commentaires
sur plusieurs lignes
-}  
-}

cte1 :: Int
cte1 = 1
       
cte2 :: Int
cte2 = 2

cte3 :: Int
cte3 = cte1+cte2

uneConstante :: Int
uneConstante = 1+2*3
--uneConstante = 1+(2*3)
--uneConstante = (1+2)*3

--mySub :: (Int -> Int) -> Int
---mySub :: Int -> (Int -> Int)
mySub :: Int -> Int -> Int
mySub x y = x-y

toto :: Int
--toto = mySub 1 2
--toto = 1 `mySub` 2
--toto = 1 - 2
toto = (-) 1 2

myNeg :: Int -> Int
myNeg = mySub 0    -- application partielle, eta reduction

myNeg' :: Int -> Int
myNeg' x = mySub 0 x   

l0 :: [Int]
l0 = []

l1 :: [Int]
l1 = 1:l0 -- cons, constructeur, binaire, (:) :: Int -> [Int] -> [Int]

l2 :: [Int]
--l2 = [1..10]
--l2 = [1,3..10]
l2 = 54:[10,8..0]
     
myHead :: [Int] -> Int
myHead (x:_) = x 

myTail :: [Int] -> [Int]
myTail (_:xs) = xs

-- les fonctions se composent

b1 :: Bool
b1 = not (True && False)

b2 :: Bool
b2 = 1==2

myAppend :: [Int] -> [Int] -> [Int]
myAppend (x:xs) ys = x:myAppend xs ys
myAppend []     ys = ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys | not (null xs) = head xs:myAppend' (tail xs) ys
                | otherwise     = ys
--                | True          = ys
--                | null xs       = ys

myAppend3 :: [Int] -> [Int] -> [Int]
myAppend3 (x:xs) ys = let suite = myAppend3 xs ys
                      in x:suite
myAppend3 []     ys = ys


myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x:xs) ys = x:suite
                      where suite = myAppend4 xs ys
myAppend4 []     ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 xs ys = append5' xs 
    where append5' (x:xs) = x:append5' xs
          append5' []     = ys

myLength :: [Int] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0


                     
