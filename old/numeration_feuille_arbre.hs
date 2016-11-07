

test1 = listToBinTree "abcde"
test2 = fst (numberLeaves 0 test1)
        
data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

listToBinTree :: [a] -> BinTree a
listToBinTree [a] = Leaf a
listToBinTree as = let (ls,rs) = splitAt (length as `div` 2) as in Node (listToBinTree ls) (listToBinTree rs)
                                 
numberLeaves :: Int -> BinTree a -> (BinTree (a,Int),Int)
numberLeaves n (Leaf i) = (Leaf (i,n),n+1)
numberLeaves n (Node l r) = let (l',n1) = numberLeaves n l
                                (r',n2) = numberLeaves n1 r
                            in (Node l' r',n2)