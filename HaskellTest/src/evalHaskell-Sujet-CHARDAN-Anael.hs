
-- NOM : CHARDAN
-- Prenom : Anael
-- TP NOTE HASKEL FIL A2
-- detection de n-gram commun a deux documents 

-- fonctions autorisees : (.), (++), (==), (/=), (<), (>=), concat, drop, dropWhile, elem, filter, foldr, head, init, isSpace, last, length, map, not, null, tail, take, takeWhile, toUpper

import Data.Char (toUpper,isSpace)

-- Q1 les prefixes de longueur croissante
testQ1 = prefixesRec "abc" == ["","a","ab","abc"]
prefixesRec :: [a] -> [[a]]
prefixesRec [x] = [[],[x]]
prefixesRec [] = [[]]
prefixesRec (x:xs) = prefixesRec (x:init xs) ++ [x:xs]

-- Q2 les suffixes de longueur croissante
testQ2 = suffixesRec "abc" == ["abc","bc","c",""]
suffixesRec :: [a] -> [[a]]
suffixesRec [x] = [[x], []]
suffixesRec [] = [[]]
suffixesRec (x:xs) = (x:xs):suffixesRec xs

-- Q3 les segments
testQ3 = segmentsNonRec "abc" == ["","a","ab","b","abc","bc","c"]
segmentsNonRec :: [a] -> [[a]]
segmentsNonRec x = [] : foldr (\y acc -> init (suffixesRec y)  ++  acc) [] (prefixesRec x)

-- Q4 supprime les doublons
testQ4 = listToSetRec ["","a","ab","b","abc","bc","c","abcb","bcb","cb","b","abcbc","bcbc","cbc","bc","c"] == ["","a","ab","b","abc","bc","c","abcb","bcb","cb","abcbc","bcbc","cbc"]
listToSetRec :: Eq a => [a] -> [a]
listToSetRec [] = []
listToSetRec (x:xs) =  x : listToSetRec (filter(/= x) xs)

-- Q5 intersection ensembliste
testQ5 = interNonRec "abc" "bccad" == "bcca"
interNonRec :: Eq a => [a] -> [a] -> [a]
interNonRec s = filter (`elem` s)

-- Q6 tri rapide decroissant selon la metrique passee en parametre
-- Désolé je n'ai pas eu le temps de m'y attarder
testQ6 = qSortByRec length  ["","a","ab","b","abc","bc","c","abcb","bcb","cb","abcbc","bcbc","cbc"] == ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""]
qSortByRec :: (a -> Int) -> [a] -> [a]
qSortByRec = undefined

-- Q7 test is le premier argument est le prefixe du second
-- Cette réponse n'est pas correct pour le testQ7'
testQ7 = isPrefixNonRec ["abcbc","abcb","bcbc"] ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""] == True
testQ7' = isPrefixNonRec ["abcb","bcbc"] ["abcbc","abcb","bcbc","abc","bcb","cbc","ab","bc","cb","a","b","c",""] == False
isPrefixNonRec :: Eq a => [a] -> [a] -> Bool
isPrefixNonRec x1 x2 = x1 == (take (length x1) x2)

-- Q8 remplace dans le deuxieme arg les occurrences du premier en les passant en majuscule
testQ8 = replaceRec [" ","texte"," ","a"," ","analyser"] ["voici"," ","un"," ","texte"," ","a"," ","analyser"] == ["voici"," ","un"," ","TEXTE"," ","A"," ","ANALYSER"]
replaceRec :: [String] -> [String] -> [String]
replaceRec _  [] = []
replaceRec []  _ = []
replaceRec x (y:ys) | y `elem` x    = map toUpper y : replaceRec x ys
                    | otherwise = y : replaceRec x ys

--Q9 coupe une chaine a chaque espace (les espaces multiples sont ramenes a un seul)
testQ9 = tokenizeRec "voici un  texte a   analyser" == ["voici"," ","un"," ","texte"," ","a"," ","analyser"]
tokenizeRec :: String -> [String]
tokenizeRec [] = []
tokenizeRec (x:xs) | isSpace x = " " : tokenizeRec (dropWhile isSpace xs)
                   | otherwise = (x : takeWhile (not . isSpace) xs) : tokenizeRec (dropWhile (not .isSpace) xs)

--Q10 colle les chaines
-- Pas eu le temps
testQ10 = unTokenizeNonRec ["voici"," ","un"," ","TEXTE"," ","A"," ","ANALYSER"] == "voici un TEXTE A ANALYSER"
unTokenizeNonRec :: [String] -> String
unTokenizeNonRec = undefined

-- met en majuscule la sequence la plus longue commune (sans tenir compte du nombre d'espaces
mainTest =
    let ref = tokenizeRec "voici un petit texte a analyser"
        plagiat = tokenizeRec "voici un  texte a   analyser"
        longestNGram = head (qSortByRec length (listToSetRec (interNonRec (segmentsNonRec ref) (segmentsNonRec plagiat))))
    in unTokenizeNonRec (replaceRec longestNGram plagiat) == "voici un TEXTE A ANALYSER"

