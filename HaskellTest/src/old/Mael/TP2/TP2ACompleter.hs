
-- questions ?

-- on generalise (autant que possible) le type des fonctions du bloc1

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs 
    where -- myAppend' :: [a] -> [a]
          myAppend' (x:xs) = x:myAppend' xs
          myAppend' [] = ys  

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse []     = []

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat []       = []

myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [c] -> [c]
myDrop 0 xs     = xs
myDrop _ []     = []
myDrop n (x:xs) = myDrop (n-1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:_)  0 = x
myBangBang (_:xs) n = myBangBang xs (n-1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x (y:ys) | x>y       = y:myInsert x ys
                  | otherwise = x:y:ys
myInsert x [] = [x]

--fact :: Integer -> Integer
fact 0 = 1
fact n = fact (n-1) * n

mySort :: Ord a => [a] -> [a]
mySort (x:xs) = myInsert x (mySort xs)
mySort []     = []

-- NEW STUFF

-- ordre superieur

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs) | p x       = x:myTakeWhile p xs
                     | otherwise = []
myTakeWhile p []                 = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose :: (b -> c)-> (a -> b) -> a -> c
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x:myMap f xs
myMap f []     = []

test1 = myMap odd [1..10]

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x:xs) = (map (x:) ys) ++ ys where ys = sousListes xs
sousListes []     = [[]]
                                                   
-- une fonction plus generale: foldr
-- inferer le type de foldr
-- forme graphique de la liste en peigne
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x:xs) = x `f` (foldr f k xs)
--myFoldr f k (x:xs) = f x (foldr f k xs)
myFoldr f k []     = k

myAnd' :: [Bool] -> Bool
myAnd' = undefined

-- definir reverse avec foldr
myReverse' :: [a] -> [a]
myReverse' = undefined

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' = undefined

add'' :: Int -> Int -> Int
add'' = undefined

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' = undefined

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = undefined

-- un "nouveau type" String


-- un nouveau type tuples

myFst :: (a,b) -> a
myFst = undefined

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p (x:xs) | p x       = myDropWhile p xs
myDropWhile p (x:xs) | otherwise = xs

myElem :: Eq a => a -> [a] -> Bool
myElem elem (x:xs) = elem == x || myElem elem xs
myElem _ []        = False

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem elem (x:xs) | elem == x = False
                      | otherwise = True && (myNotElem elem xs)
myNotElem _ []                    = True

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs) | p x       = x:(myFilter p xs)
                  | otherwise = myFilter p xs
myFilter _ []                 = []

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt n l = aux n l ([],[])
  where aux 0 l (as,bs)      = (reverse as,l++bs)
        aux n (x:xs) (as,bs) = aux (n-1) xs (x:as,bs)
  
myZip :: [a] -> [b] -> [(a,b)] 
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
myZip [] _          = []
myZip _ []          = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f (x:xs) (y:ys) = (f x y):(myZipWith f xs ys)
myZipWith _ [] _          = []
myZipWith _ _ []          = []

curryTest :: Num a => (a,a) -> a
curryTest (x,y) = x + y

uncurryTest :: Num a => a -> a -> a
uncurryTest x y = x + y

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f x y = f (x,y)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (x,y) = f x y 

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' f l r = map (myUncurry f) (myZip l r)

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip xs = aux xs [] []
  where aux [] l r = (reverse l,reverse r)
        aux ((a,b):xs) l r = aux xs (a:l) (b:r)

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' l = foldr (++) [] l

myMap' ::  (a -> b) -> [a] -> [b]
myMap' f l = foldr ((:) . f) [] l

myOr' ::  [Bool] -> Bool
myOr' l = foldr (||) False l

myAny :: (a -> Bool) -> [a] -> Bool
myAny p l = foldr ((||) . p) False l

myAll :: (a -> Bool) -> [a] -> Bool
myAll p l = foldr ((&&) . p) True l

myProduct :: [Int] -> Int
myProduct l = foldr (*) 1 l

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

isPremiers :: Int -> Bool
isPremiers n = all (\x -> (rem n x /= 0)) [2..(n-1)]

premiers :: [Int]
premiers = filter isPremiers [2..]

test2 = take 50 premiers

