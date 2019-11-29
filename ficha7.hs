Import Tip.Types
data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = (calcula a)+(calcula b)
calcula (Mult a b) = (calcula a)*(calcula b)
calcula (Menos a b) = (calcula a)-(calcula b)

infixa :: ExpInt -> String
infixa (Const a) = show a
infixa (Simetrico a) = show (-(calcula a))
infixa (Mais a b) = show ((calcula a) + (calcula b))
infixa (Menos a b) = show ((calcula a) - (calcula b))
infixa (Mult a b) = show ((calcula a) * (calcula b))

{-
posfixa :: ExpInt -> String
posfixa (Const a) = [a]
posfixa (Simetrico a) = show ('-' : posfixa a)
posfixa (Mais a b) = show (posfixa a ++ posfixa b ++ '+')
posfixa (Menos a b) = show (posfixa a ++ posfixa b ++ '-')
posfixa (Mult a b) = show (posfixa a ++ posfixa b ++ '*') -}

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R a filhos) = a + sum (map soma filhos)

altura :: RTree a -> Int 
altura (R x filhos) = 1 + myMaximum (map altura filhos)
                where myMaximum [] = 0
                      myMaximum l = maximum l

prune :: Int -> (RTree a) -> (RTree a)
prune 1 (R n _) = R n []
prune x (R n filhos) = R n (map (prune (x - 1)) filhos)

mirror :: RTree a -> RTree a
mirror (R a f) = R a (reverse (map mirror f))

postorder :: RTree a -> [a]
postorder (R x l) = concat (map postorder l) ++ [x]


data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show
data Ftree a = Leaf b | No a (Ftree e b) (Ftree e d) deriving Show

t = (No 'i' (No 'a' (No 'b' (Leaf 1 ) (Leaf 5)) (Leaf 4))(No '3' (Leaf 8) (Leaf 7)))

split :: Ftree a b -> (Btree a, LTree b)
split (Leaf x) = (Empty,Tip x)
split (No y e d) = let (bte,lte) = split e
                       (btd,ltd) = split d
                       in (Node y bte btd , Fork lte ltd)
-- ftree e uma arvoe com as folhas com tipos diferentes dos nodos ,neste caso as folhas são binarias e os nodos são letras--

-- exprimentar outras formas mais compactas de escrever esta função--
joinTress :: BTree a -> LTree b -> Maybe (Ftree a b)
joinTress _ (tip x) = Just leaf x
joinTress Empty _ = Nothing
joinTress (Node y e d) (Fork e1 d1) = let fte = joinTress e e1
                                          ftd = joinTress d d1
                                       in case fte of
                                       	Nothing -> Nothing
                                       	(Just te) -> Case ftd of
                                       		Nothing -> Nothing
                                       		(Just td) -> Just (No y te ted)



























































