import Data.Char
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show


altura :: BTree a -> Int
altura Empty = 0                                
altura (Node a esq dir) = 1 + max (altura esq) (altura dir)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a esq dir) = 1 + (contaNodos esq) + (contaNodos dir)

folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a esq dir) = (folhas esq) + (folhas dir)

prune1 :: Int -> BTree a -> BTree a
prune1 0 (Node a esq dir) = Empty
prune1 b Empty = Empty
prune1 b (Node a esq dir) = (Node a (prune1 (b-1) esq) (prune1 (b-1) dir))

path :: [Bool] -> BTree a -> [a]
path (True:t) (Node a esq dir) = a:path t esq
path (False:t) (Node a esq dir) = a:path t dir
path [True] Empty = []
path [False] Empty = []
path [] (Node a esq dir) = []
path [] Empty = []

mirror :: BTree a -> BTree a
mirror (Node a esq dir) = (Node a (mirror dir) (mirror esq))
mirror Empty = Empty

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f (Node a esq dir) (Node b esq1 dir1) = (Node (f a b) (zipWithBT f esq esq1) (zipWithBT f dir dir1))


folhas1 :: BTree a -> [a]
folhas1 Empty = []
folhas1 (Node a esq dir) = [a] ++ (folhas1 esq) ++ (folhas1 dir)

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node x Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node x l r) = let a = minimo l
                           bt = Node x (semMinimo l) r
                       in (a,bt)


--remove :: Ord a => a -> BTree a -> BTree a
--remove x Empty = Empty
--remove x (Node y e d) 
-- | x<y = (Node y (remove x e) d)
-- | x>y = (Node y e (remove x d))
-- | (e==Empty) = d 
-- | (d==Empty) = e
-- | otherwise = let (m,d1) = minSmin d
--               in (Node m e d1)



type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno 

a2 = (Node (855489,"Joao",TE,Rep)(Node (54684,"Antonio",TE,Rep) Empty Empty) Empty) 

inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (x,_,_,_) l r) | num == x = True 
                                 | num > x = inscNum num r 
                                 | num < x = inscNum num l

inscNome :: Nome -> Turma -> Bool
inscNome nome Empty = False
inscNome nome (Node (_,x,_,_) l r) | nome /= x = inscNome nome l || inscNome nome r
                                   | nome == x = True 

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (x,y,TE,_) l r) = (trabEst l) ++ [(x,y)] ++ trabEst r
trabEst (Node (_,_,_,_) l r) = trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (x,y,z,w) l r) = if num == x 
                                then Just w 
                                else if (num < x) 
                                     then nota num l 
                                     else nota num r

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas node = (percFaltas_Aux node) / (fromIntegral (contaNodos node)) * 100
            where percFaltas_Aux :: Turma -> Float 
                  percFaltas_Aux (Node (_,_,_,Faltou) l r) = 1 + percFaltas_Aux l + percFaltas_Aux r 
                  percFaltas_Aux (Node (_,_,_,_) l r) = percFaltas_Aux l + percFaltas_Aux r

mediaAprov :: Turma -> Float
mediaAprov node = (notaAprov node) / (aprov node)

notaAprov :: Turma -> Float
notaAprov Empty = 0
notaAprov (Node (_,_,_,Aprov x) l r) = fromIntegral x + notaAprov l + notaAprov r
notaAprov (Node (_,_,_,_) l r) = notaAprov l + notaAprov r

aprov :: Turma -> Float
aprov Empty = 0
aprov (Node (_,_,_,Aprov x) l r) = 1 + aprov l + aprov r
aprov (Node (_,_,_,_) l r) = aprov l + aprov r

aprovAv :: Turma -> Float
aprovAv Empty = 0 
aprovAv (Node (_,_,_,Aprov x) l r) = 1 + aprovAv l + aprovAv r
aprovAv (Node (_,_,_,Rep) l r) = (-1) + aprovAv l + aprovAv r
aprovAv (Node (_,_,_,_) l r) = aprovAv l + aprovAv r

mapBtree :: (a -> b) -> BTree a -> BTree b
mapBtree f Empty = Empty
mapBtree f (Node a e d) = Node (f a) (mapBtree f e) (mapBtree f d)

bonus :: Turma -> Turma
bonus t = mapBtree aux t
     where aux :: Aluno -> Aluno
           aux (nu,no,r,Aprov x) = if (x == 20) then (nu,no,r,Aprov x)  else (nu,no,r,Aprov (x+1))
           aux a = a

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

posfixa :: ExpInt -> String
posfixa (Const a) = [a]
posfixa (Simetrico a) = show ('-' : posfixa a)
posfixa (Mais a b) = show (posfixa a ++ posfixa b ++ '+')
posfixa (Menos a b) = show (posfixa a ++ posfixa b ++ '-')
posfixa (Mult a b) = show (posfixa a ++ posfixa b ++ '*') 