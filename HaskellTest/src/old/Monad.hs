

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

--numberLeaves' :: BinTree a -> Int -> (BinTree (a,Int),Int)
numberLeaves' :: BinTree a -> State Int (BinTree (a,Int))
numberLeaves' (Leaf i)   = do
  n <- myGet
  myPut (n+1)
  return (Leaf (i,n))
numberLeaves' (Node l r) = do
  l' <- numberLeaves' l
  r' <- numberLeaves' r
  return (Node l' r')

--type State s a = s -> (s,a)

data State s a = State (s -> (s,a))

unState :: State s a -> (s -> (s,a))
unState (State f) = f
               
myReturn :: a -> State s a
myReturn x = State (\s -> (s,x))

myGet :: State s s
myGet = State (\s -> (s,s))

myPut :: s -> State s ()
myPut sNew = State (const (sNew, ()))

myThen :: State s a -> (a -> State s b) -> State s b
myThen (State m) f = State (\s1 -> let (s2,x) = m s1
                                       State g = f x
                                   in g s2)

myEval :: s -> State s a -> a
myEval s0 (State f) = snd (f s0)

myRun :: s -> State s a -> s
myRun s0 (State f) = fst (f s0)

prog :: State Int ()
prog =
--    myThen myGet (\i -> myPut (i+1))
    myGet            `myThen ` (\i ->
    myPut (i+1))

prog' :: State Int ()
prog' = do
    i <- myGet
    myPut (i+1)

test3 = myRun 0 prog

instance Monad (State s) where
    return = myReturn
    (>>=) = myThen
