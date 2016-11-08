import Data.List
import Data.Ord

-- ZF expressions / liste en comprehension

rectangle :: [(Int,Int)]
rectangle = [ (x,y)| x <- [1..4], y <-[1..5]]

triangle :: [(Int,Int)]
triangle = [ (x,y)| x <- [1..4], y <-[1..5], y<=x]

triangle' :: [(Int,Int)]
triangle' = [ (x,y)| x <- [1..4], y <-[1..x]]

myQSort :: Ord a => [a] -> [a]
myQSort (x:xs) = myQSort [ e | e <- xs, e<=x] ++ x : myQSort [ e | e <- xs, e>x ] 
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
--partitionStricte (x:xs) = ([x],xs) : map (\(p1,p2) -> (x:p1,p2)) (partitionStricte xs)
partitionStricte (x:xs) = ([x],xs) : [ (x:p1,p2) | (p1,p2) <- partitionStricte xs]
                          
-- I) generate and test (brute force)

data Op = Add | Sub | Mul | Div deriving (Eq,Enum)

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

validOp :: Op -> Int -> Int -> Bool
validOp Sub x y = x > y
validOp Div x y = y > 0 && x `mod` y == 0
validOp _   _ _ = True
                  
evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div

data Exp = Val Int | App Op Exp Exp 
            deriving Show

-- step1: enumerate expressions
exps :: [Int] -> [Exp]
exps [n] = [ Val n ]
exps ns  = 
    [ App o g d 
    | (gs,ds) <- partitionStricte ns
    , g       <- exps gs
    , d       <- exps ds
    , o       <- [Add .. Div]
    ]

-- step2: filter out invalid expressions

evalExp :: Exp -> Int
evalExp (Val n) = n
evalExp (App op exp1 exp2) = (evalOp op) (evalExp exp1) (evalExp exp2)

validExp :: Exp -> Bool
validExp (Val n) = n > 0
validExp (App op exp1 exp2) = validExp exp1 && validExp exp2 && validOp op (evalExp exp1) (evalExp exp2)

solutions :: [Int] -> Int -> [Exp]
solutions cartes nb = filter ((==nb) . evalExp) (filter validExp (concat (map exps (permSousListes cartes))))

test1 = solutions [1,3,7,10,25,50] 765


-- II) fusionner la generation et le filtrage des expressions invalides

exps2 :: [Int] -> [Exp]
exps2 [n] = [Val n]
exps2 ns  = [App o g d |
             (gs,ds) <- partitionStricte ns,
             g <- exps2 gs,
             d <- exps2 ds,
             o <- [Add .. Div],
             validOp o (evalExp g) (evalExp d)]

solutions2 :: [Int] -> Int -> [Exp]
solutions2 nombres cible = filter ((==cible).evalExp) (concat (map exps2 (permSousListes nombres)))

test2 = solutions2 [1,3,7,10,25,50] 765


-- III) memoiser l'evaluation

data Exp' = Val' Int | App' Op Exp' Exp' Int 

evalExp' :: Exp' -> Int
evalExp' (Val' n) = n
evalExp' (App' _ _ _ n) = n

exps3 :: [Int] -> [Exp']
exps3 [n] = [Val' n]
exps3 ns  = [App' o g d (evalOp o (evalExp' g) (evalExp' d)) |
             (gs,ds) <- partitionStricte ns,
             g <- exps3 gs,
             d <- exps3 ds,
             o <- [Add .. Div],
             validOp o (evalExp' g) (evalExp' d)]

solutions3 :: [Int] -> Int -> [Exp']
solutions3 nombres cible = filter ((==cible).evalExp') (concat (map exps3 (permSousListes nombres)))

test3 = solutions3 [1,3,7,10,25,50] 765


-- IV) exploiter des proprietes arithmetiques

-- pour reduire l'espace de recherche on ajoute les regles :
-- - pas de multiplication par 1 
-- - pas de division par 1
-- - addition et multiplication commutatives (ne considerer qu'un sens (quand les deux operandes sont differents))
validOp' :: Op -> Int -> Int -> Bool
validOp' Add x y = x >= y
validOp' Sub x y = x > y
validOp' Div x y = y > 1 && x `mod` y == 0
validOp' Mul x y = x >= y && x > 1 && y > 1

exps4 :: [Int] -> [Exp']
exps4 [n] = [Val' n]
exps4 ns  = [App' o g d (evalOp o (evalExp' g) (evalExp' d)) |
             (gs,ds) <- partitionStricte ns,
             g <- exps4 gs,
             d <- exps4 ds,
             o <- [Add .. Div],
             validOp' o (evalExp' g) (evalExp' d)]

solutions4 :: [Int] -> Int -> [Exp']
solutions4 nombres cible = filter ((==cible).evalExp') (concat (map exps4 (permSousListes nombres)))

test4 = solutions4 [1,3,7,10,25,50] 765

-- nombre de solutions

nombreDeSolutions3 = length test3
nombreDeSolutions4 = length test4

-- V) ne retourner qu'une solution exacte ou bien la plus proche
solutions5 :: [Int] -> Int -> [Exp']
solutions5 nombres cible = [last (aux exp)]
  where exp        = concat (map exps4 (permSousListes nombres))
        aux (x:xs) = x:filter (\n -> (abs (cible-(evalExp' n))) < (abs (cible-(evalExp' x)))) (aux xs)
        aux []     = []

test5 = solutions5 [1,3,7,10,25,50] 765
test6 = solutions5 [1,3,7,10,25,50] 831

-- VI) affichez les expressions sous forme infixe en evitant des parentheses inutiles
instance Show Exp' where 
    show (Val' v) = show v
    show (App' Add g d r) = "(" ++ show g ++ show Add  ++ show d ++ ")"
    show (App' Sub g d r) = "(" ++ show g ++ show Sub ++ show d ++ ")"
    show (App' o g d r) = show g ++ show o ++ show d

-- VII) generalisez certaines fonctions avec de l'ordre superieur afin de reduire la duplication de code dans ce programme

-- misc : cherchez les solutions avec le moins d'operations en priorite
