import Data.List
import Data.Ord

-- ZF expressions / liste en comprehension

rectangle :: [(Int,Int)]
rectangle = [ (i,j) | i <- [1..n], j <- [1..m]]
            where n = 4
                  m = 5
                      
triangle :: [(Int,Int)]
triangle = [ (i,j) | i <- [1..n], j <- [1..m], j<=i]
            where n = 4
                  m = 5


triangle' :: [(Int,Int)]
triangle' = [ (i,j) | i <- [1..n], j <- [1..i]]
            where n = 4
                  m = 5

myQSort :: Ord a => [a] -> [a]
myQSort (x:xs) = myQSort [ e | e <- xs, e<=x] ++ [x] ++ myQSort [ e | e <- xs, e>x]
myQSort []     = []

                 
-- but du TP : trouver les solutions pour le compte est bon

-- sous listes et permutations pour considerer les nombres utilises dans le calcul

-- sous liste 

-- deja vu au bloc 2
sousListes :: [a] -> [[a]]
sousListes []     = [[]]
sousListes (x:xs) = ys ++ map (x:) ys
    where ys = sousListes xs

injections :: a -> [a] -> [[a]]
injections x (y:ys) = (x:y:ys) : map (y:) (injections x ys)
injections x []     = [[x]]
                      
permuts :: [a] -> [[a]]
permuts (x:xs) = concat (map (injections x) (permuts xs))
permuts []     = [[]]

permSousListes :: [a] -> [[a]]
permSousListes xs = [zs | ys <- sousListes xs, not (null ys), zs <- permuts ys]

partitionStricte :: [a] -> [([a],[a])]
partitionStricte [x1,x2] = [([x1],[x2])]
partitionStricte (x:xs)  = ([x],xs): [ (x:ls,rs) | (ls,rs) <- partitionStricte xs ]

-- I) generate and test (brute force)

data Op = Add | Sub | Mul | Div deriving Eq -- Show

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
              
validOp :: Op -> Int -> Int -> Bool
validOp Sub x y = x>y
validOp Div x y = x `mod` y == 0
validOp _   _ _ = True

evalOp :: Op -> Int -> Int -> Int
evalOp Add i1 i2 = i1+i2
evalOp Sub i1 i2 = i1-i2
evalOp Mul i1 i2 = i1*i2
evalOp Div i1 i2 = div i1 i2
                   
data Exp = Val Int | App Op Exp Exp 
            deriving Show

-- step1: enumerate expressions
exps :: [Int] -> [Exp]
exps [n] = [Val n]
exps ns =
    [ App o g d
      | (gs,ds) <- partitionStricte ns
    , g <- exps gs
    , d <- exps ds
    , o <- [Add,Sub,Mul,Div]
    ]

-- step2: filter out invalid expressions

evalExp :: Exp -> Int
evalExp (App o g d) = evalOp o (evalExp g) (evalExp d)
evalExp (Val n) = n

validExp :: Exp -> Bool
validExp (Val n) = n>0
validExp (App o g d) = validExp g && validExp d && validOp o (evalExp g) (evalExp d)

solutions :: [Int] -> Int -> [Exp]
solutions nombres cible =
    let ns = permSousListes nombres
        es = concat (map exps ns)
        es' = filter validExp es
        es'' = filter (\e -> evalExp e == cible) es'
    in es''
           
test1 = solutions [1,3,7,10,25,50] 765


-- II) fusionner la generation et le filtrage des expressions invalides

exps2 :: [Int] -> [Exp]
exps2 = undefined

solutions2 :: [Int] -> Int -> [Exp]
solutions2 nombres cible = undefined

test2 = solutions2 [1,3,7,10,25,50] 765


-- III) memoiser l'evaluation

data Exp' = Val' Int | App' Op Exp' Exp' Int 
            deriving Show

evalExp' :: Exp' -> Int
evalExp' (Val' n) = n
evalExp' (App' _ _ _ n) = n

exps3 :: [Int] -> [Exp']
exps3 = undefined

solutions3 :: [Int] -> Int -> [Exp']
solutions3 nombres cible = undefined

test3 = solutions3 [1,3,7,10,25,50] 765


-- IV) exploiter des proprietes arithmetiques

-- pour reduire l'espace de recherche on ajoute les regles :
-- - pas de multiplication par 1 
-- - pas de division par 1
-- - addition et multiplication commutatives (ne considerer qu'un sens (quand les deux operandes sont differents))
validOp' :: Op -> Int -> Int -> Bool
validOp' o x y = undefined

exps4 :: [Int] -> [Exp']
exps4 = undefined

solutions4 :: [Int] -> Int -> [Exp']
solutions4 nombres cible = undefined

test4 = solutions4 [1,3,7,10,25,50] 765

-- nombre de solutions

nombreDeSolutions3 = length test3
nombreDeSolutions4 = length test4

-- V) ne retourner qu'une solution exacte ou bien la plus proche 

solutions5 :: [Int] -> Int -> [Exp']
solutions5 nombres cible = undefined

test5 = solutions5 [1,3,7,10,25,50] 765
test6 = solutions5 [1,3,7,10,25,50] 831

-- VI) affichez les expressions sous forme infixe en evitant des parentheses inutiles
--instance Show Exp' where 
--    show :: Exp' -> String
--    show e = undefined

-- VII) generalisez certaines fonctions avec de l'ordre superieur afin de reduire la duplication de code dans ce programme

-- misc : cherchez les solutions avec le moins d'operations en priorite
