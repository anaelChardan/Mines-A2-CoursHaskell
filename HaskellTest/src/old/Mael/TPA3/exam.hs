-- vous pouvez utiliser : (++), length, splitAt, div, (.), filter, foldr, map, fst, snd, et les fonctions de ce TP

-- si vous etes coinces faites autrement mais c'est moins bien

-- pour eviter les pb de type convertir : 1/(fromIntegral 12)

-- map as an ordered binary tree
-- left subtrees contain lower keys and right subtrees contain greater keys

data Map k v = Node k v (Map k v) (Map k v) | Empty deriving (Show,Eq)

-- Q1) (def rec) return the mapping of the key (crash if the key does not exist)
-- readM "A" (Node "C" 0.25 (Node "B" 0.35 (Node "A" 0.45 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)) == 0.45
readM :: Ord k => k -> Map k v -> v
readM a (Node k v l r) | a == k = v
| a < k = readM a l
| otherwise = readM a r

-- Q2) (def rec) update the value of a key (insert the value if the key does not exist)
-- writeM "A" 0.1 (Node "C" 0.25 (Node "B" 0.35 (Node "A" 0.45 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)) == Node "C" 0.25 (Node "B" 0.35 (Node "A" 0.1 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
-- writeM "A" 0.1 (Node "C" 0.25 (Node "B" 0.35 Empty Empty) (Node "D" 0.25 Empty Empty)) == Node "C" 0.25 (Node "B" 0.35 (Node "A" 0.1 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
writeM :: Ord k => k -> v -> Map k v -> Map k v
writeM key val Empty = (Node key val Empty Empty)
writeM key val (Node k v l r) | key == k = (Node key val l r)
| key < k = (Node k v (writeM key val l) r)
| otherwise = (Node k v l (writeM key val r))

-- Q3) (def rec) list the pair (key,value) in increasing oder of keys
-- toListM (Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)) == [("A",0.25),("B",0.25),("C",0.25),("D",0.25)]
toListM :: Map k v -> [(k,v)]
toListM Empty = []
toListM (Node k v l r) = toListM l ++ [(k,v)] ++ toListM r

-- Q4) (def rec) build a balanced tree from a list of pairs sorted by increasing keys
-- fromListM [("A",0.25),("B",0.25),("C",0.25),("D",0.25)] == Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
fromListM :: [(k,v)] -> Map k v
fromListM [] = Empty
fromListM ((key,val):[]) = Node key val Empty Empty
fromListM gs =
let ((x:xs), (y:ys)) = splitAt ((length gs) `div` 2) gs
(newK, newV) | length (y:ys) == 0 = x
| otherwise = y
in Node newK newV (fromListM (x:xs)) (fromListM ys)

-- Q5) (def NO rec) balance a tree
-- balanceM (Node "D" 0.25 (Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) Empty) Empty) == Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
balanceM :: Ord k => Map k v -> Map k v
balanceM Empty = Empty
balanceM a = fromListM (toListM a)

-- page ranking according to google
-- source: http://en.wikipedia.org/wiki/PageRank

-- simplified algorithm

-- graph as a list of successors

type Successors a = [a]

type Node a = (a,Successors a)

type Graph a = [Node a]

-- here we use String instead of URL

g1 :: Graph String
g1 =
[("A",[])
,("B",["A"])
,("C",["A"])
,("D",["A"])
]

g2 :: Graph String
g2 =
[("A",[])
,("B",["A","C"])
,("C",["A"])
,("D",["A","B","C"])
]

g3 :: Graph String
g3 =
[("A",[])
,("B",["C","A","C"])
,("C",["C","A","C"])
,("D",["A","B","C"])
]

-- rule 1: links from a page to itself are ignored.

--Q6) (def NO rec) implement the rule 1
ignoreSelfNode :: Eq a => Node a -> Node a
ignoreSelfNode (a, xs) = (a,(filter (\x -> a /= x) xs))

-- ignoreSelf g3 == [("A",[]),("B",["C","A","C"]),("C",["A"]),("D",["A","B","C"])]
ignoreSelf :: Eq a => Graph a -> Graph a
ignoreSelf = map ignoreSelfNode

-- rule 2: multiple outbound links from one single page to another single page are ignored

--Q7) (def REC) delete multiple occurences
-- myNub [1,3,2,1,3] == [1,3,2]
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (x:xs) = x : myNub (filter (/=x) xs)

--Q8) (def NO rec) implement the rule 2
ignoreMultipleNode :: Eq a => Node a -> Node a
ignoreMultipleNode (a, xs) = (a,myNub xs)

-- ignoreMultiple g3 == [("A",[]),("B",["C","A"]),("C",["C","A"]),("D",["A","B","C"])]
ignoreMultiple :: Eq a => Graph a -> Graph a
ignoreMultiple = map ignoreMultipleNode

-- rule 3: pages with no outbound links are assumed to link out to all other pages

--Q9) (def NO rec) implement rule 3
completeSinkNode :: Eq a => [a] -> Node a -> Node a
completeSinkNode ls (a,[]) = ignoreSelfNode (a,a:ls)
completeSinkNode ls a = a

-- completeSink g3 == [("A",["B","C","D"]),("B",["C","A","C"]),("C",["C","A","C"]),("D",["A","B","C"])]
completeSink :: Eq a => Graph a -> Graph a
completeSink gs = map (completeSinkNode ids) gs
where ids = map fst gs

-- assume an even probability distribution between 0 and 1

--Q10) (def NO rec) tous les noeuds ont la meme probalite initialement, la somme des proba est 1
-- initEvenProba g2 == Node "D" 0.25 (Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) Empty) Empty
initEvenProba :: Ord a => Graph a -> Map a Float
initEvenProba (x:xs) = fromListM liste
where proba = 1/(fromIntegral (length (x:xs)))
liste = map (\x -> (x,proba)) (map fst (x:xs))


-- initPR g2 == Node "C" 0.25 (Node "B" 0.25 (Node "A" 0.25 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
initPR :: Ord a => Graph a -> Map a Float
initPR = balanceM . initEvenProba . completeSink . ignoreMultiple . ignoreSelf

-- crawl and update page rank

updateNodePR :: Ord a => Map a Float -> Node a -> Map a Float -> Map a Float
updateNodePR prOld (n,ns) pr = foldr (\k -> writeM k (delta+readM k pr)) pr ns
where delta = readM n prOld / fromIntegral (length ns)

--Q11) (def NO rec)
-- updatePR g2 (initPR g2) == Node "C" 0.45833334 (Node "B" 0.33333334 (Node "A" 0.7083334 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
updatePR :: Ord a => Graph a -> Map a Float -> Map a Float
updatePR = undefined
{--updatePR xs m = foldr (\a -> updateNodePR m a m) Empty xsPr
where xsPr = foldr (calPr xs) [] xs
calPr ::
--}
-- NON TERMINE

-- Q12) (def rec) doit faire la meme chose que la fonction iterate predefinie
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:myIterate f (f a)

-- crawl g2 2 == Node "C" 0.7083334 (Node "B" 0.4166667 (Node "A" 1.4166666 Empty Empty) Empty) (Node "D" 0.25 Empty Empty)
crawl :: Ord a => Graph a -> Int -> Map a Float
crawl g n = myIterate (updatePR g) (initPR g) !! n

-- Bonus : la definition de crawl ci dessus a un pb, lequel ? (Reponse en 15 mots maxi)
-- reponse :
