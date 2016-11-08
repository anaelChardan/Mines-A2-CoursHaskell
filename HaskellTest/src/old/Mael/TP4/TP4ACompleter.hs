
-- NOM : NACCACHE 
-- PRENOM : MAËL


-- compression de huffman

exemple1 :: String
exemple1 = "abcbcaa"

-- arbre de Huffman

data Huff a = Leaf a | Node (Huff a) (Huff a) deriving (Show,Eq)

-- chemin dans un arbre

type Bits = [Bit]

data Bit = L | R deriving (Show,Eq)

-- construction de l'arbre de Huffman


-- Q1 (def recursive)
-- calcule pour chaque caractere d'une chaine son nombre d'occurences
-- nbOccurrences exemple1 == [('a',3),('b',2),('c',2)]
nbOccurrences :: String -> [(Char,Int)]
nbOccurrences (x:xs) = [(x, 1+length (filter (\c -> c==x) xs))] ++ filter (\(c,_) -> c/=x) (nbOccurrences xs)
nbOccurrences []     = []


-- Q2 (def recursive)
-- le poids est la somme des occurences contenues dans ses feuilles
-- poids (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == 7
poids :: Huff (a,Int) -> Int
poids (Leaf (_,x)) = x
poids (Node l r)   = poids l + poids r

-- Q3 (def recursive)
-- insere un arbre a sa place dans une liste d'arbres tries par poids croissants
-- insere (Leaf ('a',3)) [Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
insere :: Huff (a,Int) -> [Huff (a,Int)] -> [Huff (a,Int)] 
insere t1 (t:ts) | poids t1 > poids t = t:insere t1 ts
                 | otherwise          = t1:t:ts
insere t1 []                          = [t1]


-- Q4 (def non recursive avec foldr)
-- tri des arbres de Huffman par poids croissants
-- triHuff [Leaf ('a',3),Leaf ('b',2),Leaf ('c',2)] == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
triHuff :: [Huff (a,Int)] -> [Huff (a,Int)]
triHuff (t:ts) = insere t (triHuff ts)
triHuff []     = []

-- Q5 (def non recursive)
-- construit la liste des feuilles pour une chaine donnee
-- feuilles exemple1 == [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)]
feuilles :: String -> [Huff (Char,Int)]
feuilles str = triHuff (map (\x -> Leaf x) (nbOccurrences str))

-- Q6 (def recursive)
-- agreege les deux arbres les plus legers, repetitivement pour obtenir un arbre unique
-- agrege [Leaf ('b',2),Leaf ('c',2),Leaf ('a',3)] == Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))
agrege :: [Huff (a,Int)] -> Huff (a,Int)
agrege (l:[])     = l
agrege [l1,l2]    = Node l1 l2
agrege (l1:l2:ls) = (agrege (insere (Node l1 l2) ls)) 


-- Q7 (def recursive)
-- efface les nombre d'occurences dans un arbre de Huffman
-- strip (Node (Leaf ('a',3)) (Node (Leaf ('b',2)) (Leaf ('c',2)))) == Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))
strip :: Huff (a,Int) -> Huff a
strip (Node l r)   = Node (strip l) (strip r)
strip (Leaf (x,_)) = Leaf x

-- construit l'arbre de Huffman pour une chaine donnee
-- buildHuff exemple1 = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))
buildHuff :: String -> Huff Char 
buildHuff = strip . agrege . feuilles


-- codage d'une chaine

-- code un caractere 
-- codeOne (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 'b' == [R,L]
codeOne :: Huff Char -> Char -> Bits
codeOne root c = head (codeOne' root c)

-- Q8 (def recursive qui utilise aussi map)
-- code un caractere, retourne une liste de longueur 1 qui contient le chemin du caractere
--  codeOne' (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 'b' == [[R,L]]
codeOne' :: Huff Char -> Char -> [Bits]
codeOne' (Leaf x) c | x == c    = [[]]
                    | otherwise = []
codeOne' (Node l r) c           = (map (\x -> L:x) (codeOne' l c))
                                   ++ (map (\x -> R:x) (codeOne' r c))

-- code tous les caracteres d'une chaine
-- codeAll (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) exemple1 == [L,R,L,R,R,R,L,R,R,L,L]
codeAll :: Huff Char -> String -> Bits
codeAll root s = concat (map (codeOne root) s)


-- Q10 (def recursive)
-- decodage d'une liste de bits
-- decode (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) [L,R,L,R,R,R,L,R,R,L,L] == "abcbcaa"
decode :: Huff Char -> Huff Char -> Bits -> String
decode _ (Leaf x) []                    = [x]
decode gt (Leaf x) l                    = x:decode gt gt l
decode gt (Node l r) (x:xs) | x == L    = decode gt l xs
                            | otherwise = decode gt r xs

-- verifie la correction en codange puis decodant et calcule le ratio de compression (hors arbre)
-- test10 exemple1 == (True,0.19642857)
test10 :: String -> (Bool,Float)
test10 s = 
    let t = buildHuff s
        c = codeAll t s
        s' = decode t t c
    in (s==s',(fromIntegral (length c))/(fromIntegral (8*length s)))

-- Q10 
-- en 10 mots maximum identifier ce qui est recalcule de nombreuses fois et pourrait etre optimise dans ce code

-- reponse : 
{-

-}
