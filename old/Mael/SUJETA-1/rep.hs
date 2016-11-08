data Map k v = Node k v (Map k v) (Map k v) | Empty deriving (Show,Eq)

readM :: Ord k => k -> Map k v -> v
readM s (Node k v l r) | s == k = v
                       | s >  k = readM s r
                       | s <  k = readM s l


writeM :: Ord k => k -> v -> Map k v -> Map k v
writeM k  v  Empty          = (Node k v Empty Empty)
writeM ik iv (Node k v l r) | ik == k = (Node k iv l r)
                            | ik >  k = (Node k v l (writeM ik iv r))
                            | ik <  k = (Node k v (writeM ik iv l) r)

toListM :: Map k v -> [(k,v)]
toListM Empty = []
toListM (Node k v l r) = toListM l ++ [(k,v)] ++ toListM r

fromListM :: [(k,v)] -> Map k v
fromListM [] = Empty
fromListM l  = (aux (splitAt (div (length l) 2))
  where aux ((x:xs),(y:ys)) 
