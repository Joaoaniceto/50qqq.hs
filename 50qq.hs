import Data.Maybe
import Data.Char
import Prelude

enumFromToo :: Int -> Int -> [Int]
enumFromToo x y = [x..y]
enumFromThenToo :: Int -> Int -> Int -> [Int]
enumFromThenToo x y z = [x,y..z]

--(+++) :: [a] -> [a] -> [a]
--(+++) (h1) (h2) = h1:h2

(!!!) :: [a] -> Int -> a
(!!!) (h:s) 0 = h 
(!!!) (h:s) a = (!!!) s (a-1)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:s) = reverse1 s ++ [h]

take1 :: Int -> [a] -> [a]
take1 a [] = []
take1 0 a = [] 
take1 a (h:s) = h:take1 (a-1) s

drop1 :: Int -> [a] -> [a] 
drop1 a [] = []
drop1 0 a = a
drop1 a (h:s) = drop1 (a-1) s 

zip1 :: [a] -> [b] -> [(a,b)]
zip1 (h1:t1) (h2:t2) = (h1,h2):(zip1 t1 t2)

elem1 :: Eq a => a -> [a] -> Bool
elem1 a (h:s) = if (a==h) then True else elem1 a s
elem1 a [] = False

replicate1 :: Int -> a -> [a]
replicate1 0 y = []
replicate1 x y = y:(replicate1 (x-1) y)

intersperse1 :: a -> [a] -> [a]
intersperse1 a (h1:s) = h1:a:(intersperse1 a s)
intersperse1 a [] = []


group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [a] = [[a]]
group1 (h:h1:s) = if (h==h1) then ([h] ++ [h1]):(group1 s) else [h]:group1 (h1:s)

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 ([]:vs) = concat1 vs
concat1 ((h):vs) = h ++ concat1 (vs)

tails1 :: [a] -> [[a]]
tails1 [] = [[]] 
tails1 (h:s) = [(h:s)] ++ tails1 s

ispre :: Eq a => [a] -> [a] -> Bool
ispre [] [] = True
ispre [] l = True
ispre (h:s) (h1:s1) = if (h==h1) then (ispre s s1) else False  

issuf :: Eq a => [a]-> [a] -> Bool
issuf [] l = True
issuf l [] = False
issuf (h:s) (h1:s1) = if (h==h1) && (s==s1) then True else issuf (h:s) s1

isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] l = True
isSubsequenceOf1 l [] = False
isSubsequenceOf1 (h:t) (h1:t1) = if h==h1 then isSubsequenceOf1 t t1 else isSubsequenceOf1  (h:t) t1


elemindices :: Eq a => a -> [a] -> [Int]
elemindices x [] = []
elemindices x y = count 0 x y
 where
 count :: Eq a => Int -> a -> [a] -> [Int]
 count c x [] = []
 count c x (y:ys)
   | x == y = c:count (c+1) x ys
   | otherwise = count (c+1) x ys

contido :: Eq a => a -> [a] -> Bool
contido x [] = False
contido x (h:t) = if x==h then True else contido x t

nub :: Eq a => [a] -> [a] 
nub [] = []
nub (h:s) = if (contido h s) == False then h:(nub s) else nub s

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t) = if (x==h) then t else h:delete x (t)

deleteall :: Eq a => [a] -> [a]-> [a]
deleteall [] a = []
deleteall a [] = a
deleteall (h:s) (h1:s2) = if (h==h1) then deleteall s s2 else h:deleteall s (h1:s2) 

union1 :: Eq a => [a] -> [a] -> [a]
union1 [] l = l
union1 l [] = l
union1 (h:s) (h1:s1) = if (h == h1) then h:union1 s s1 else h:union1 s (h1:s1)

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] a = [] 
intersect1 (h:s) a = if ((contido h a) == True) then  h:intersect1 s a else intersect1 s a

insert1 :: Ord a => a -> [a] -> [a]
insert1 x xs = foldr pom poc xs False
  where
    pom y f je
      | je || x > y = y : f je
      | otherwise   = x : y : f True
    poc True = []
    poc _    = [x]

unwords1 :: [String] -> String
unwords1 [] = []
unwords1 (h:t) = h ++ " " ++ unwords1 t

unlines1 :: [String] -> String
unlines1 [] = []
unlines1 (h:s) = h ++ "\n" ++ unlines1 s


pMaior1 :: Ord a => [a]  -> Int
pMaior1 (h:t) = if (h == maiorelem (h:t)) then 0 else (pMaior1 t) + 1
             where maiorelem :: Ord a => [a] -> a
                   maiorelem [a] = a
                   maiorelem (h:h1:s) = if (h1>h) then maiorelem (h1:s) else maiorelem (h:s)

funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t


temrepetidos1 :: Eq a => [a] -> Bool
temrepetidos1 [] = False
temrepetidos1 (h:s) = if (aux h s == True ) then True else temrepetidos1 s
                      where aux :: Eq a => a -> [a] -> Bool
                            aux a [] = False
                            aux a (h:s) = if a==h then True else aux a s

posimpar :: [a] -> [a]
posimpar [] = []
posimpar (h:h1:s) = h1:(posimpar s)

posPares :: [a] -> [a]
posPares [] = []
posPares (h:h1:s) = h:(posPares s) 


isSorted1 :: Ord a => [a] -> Bool
isSorted1 [] = True
isSorted1 (h:h1:s) = if (h>h1) then False else isSorted1 (h1:s)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:s) = insert1 h (iSort s)

menor1 :: String -> String -> Bool
menor1 a b = if (aux a < aux b) then True else False
             where aux :: [a] -> Int
                   aux [] = 0
                   aux (h:s) = (aux s) + 1

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((a1,x1):s) = if (a==a1) then True else elemMSet a s

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):s) = b + lengthMSet s

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):s) = aux (a,b) ++ converteMSet s
                         where aux :: (a,Int) -> [a]
                               aux (a,0) = []
                               aux (a,b) = a:aux (a,(b-1))


insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet b [] = []
insereMSet b ((x1,x2):s) = if (b==x1) then (x1,(x2+1)):s else ((x1,x2):(insereMSet b s))

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet b [] = []
removeMSet b ((x1,x2):s) = if (b==x1) then s else (x1,x2):removeMSet b s

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:s) = (h,(aux h (h:s))):constroiMSet(drop (aux h (h:s)) (h:s) )
                     where aux :: Eq a => a -> [a] -> Int
                           aux a [] = 0
                           aux a (h:s) = if a==h then (aux a s) + 1 else (aux a s)


maybee :: [Maybe a] -> [a]
maybee (Nothing:xs) = maybee xs
maybee (Just x:xs)  = x : maybee xs
maybee []           = []

partitionEithers :: [Either a b] -> [a]
partitionEithers [] = []
partitionEithers (Left a:s) = a: partitionEithers s
partitionEithers (Right a:s) = partitionEithers s

partitionEithersd :: [Either a b] -> [b]
partitionEithersd [] = []
partitionEithersd (Left a:s) = partitionEithersd s
partitionEithersd (Right a:s) = a:partitionEithersd s

partitionEither :: [Either a b] -> ([a],[b])
partitionEither h  = (partitionEithers h , partitionEithersd h)



data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y-1)
posicao (x,y) (Norte:s) = (x,y+1)
posicao (x,y) (Sul:s) = (x,y-1)
posicao (x,y) (Este:s) = (x+1,y)
posicao (x,y) (Oeste:s) = (x-1,y)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) 
  | x<x1 = Este:caminho (x+1,y) (x1,y1)
  | x>x1  = Oeste:caminho (x-1,y) (x1,y1)
  | y>y1  = Sul:caminho (x,y-1) (x1,y1)
  | y<y1  = Norte:caminho (x,y+1) (x1,y1)
  | (x==x1 && y==y1) = []

vertical :: [Movimento] -> Bool
vertical [] = False
vertical (Oeste:s) = False
vertical (Este:s) = False
vertical (h:s) = vertical s

data Posicao = Pos Int Int deriving Show

maisCentral1 ::  [Posicao] -> Posicao
maisCentral1 [a] = a 
maisCentral1 ((Pos x y):(Pos x1 y1):s) = if ((x^2) +(y^2)) < ((x1^2)+(y1^2)) then maisCentral1 ((Pos x y):s) else maisCentral1 ((Pos x1 y1):s)


vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos a [] = []
vizinhos a (h:t) = if ((aux a h) == 1) then h:vizinhos a t else vizinhos a t
                   where aux :: Posicao -> Posicao -> Int
                         aux (Pos x y) (Pos x1 y1) = ((x-x1)^2) + ((y-y1)^2)

--mesmaOrdenada :: [Posicao] -> Bool
--mesmaOrdenada a = True
--mesmaOrdenada ((Pos x y):(Pos x1 y1):t) = if (x == x1) then mesmaOrdenada ((Pos x1 y):t) else False

data Contacto = Casa Integer 
 | Trab Integer 
 | Tlm Integer 
 | Email String deriving Show
 
type Nome = String 
type Agenda = [(Nome, [Contacto])]

--acrescEmail :: Nome -> String -> Agenda -> Agenda
--acrescEmail no em [] = [no,[Email em]]
--acrescEmail no em ((n,h):g) 
-- | no /= n = (n,h):(acrescEmail no em g)
-- | no == n = (n,(Email em):h):g


--verEmails :: Nome -> Agenda -> Maybe [String]
--verEmails _ [] = Nothing
--verEmails no ((n,h):g) 
-- | no /= n = verEmails no g
-- | no == n = just (procEmail h)
-- where procEmail :: [Contacto] -> String
--       procEmail [] = []
--       procEmail ((Email e):h)= e:procEmail h
--      procEmail (_,h) = procEmail h


--consTelefs :: [Contacto] -> [Integer]
--consTelefs [] = []
--consTelefs ((Tlm a):h) = a:consTelefs h
--consTelefs ((Trab a):h) = a:consTelefs h
--consTelefs ((Casa a):h) = a:consTelefs h


--casa :: Nome -> Agenda -> Maybe Integer
--casa no [] = Nothing
--casa no (n,h:g)= if (no /= n) then casa no g else Just procCasa h
--                                     where procCasa :: [Contacto] -> Maybe Integer
--                                          procCasa [] = Nothing
--                                           procCasa (Casa a:h) = Just a


--digitAlpha :: String -> (String,String) 
--digitAlpha [] = ([],[])
--digitAlpha (c:t)
--    | isAlpha c = (ld,c:ll)
--    | isDigit c = (c:ld,ll)
---     Otherwise = (ld,ll)
---	where(ld,ll) = digitAlpha t

--divMod :: Int -> Int -> (Int,Int)
--divMod x y
--     | x < y = (0,x)
--     | x >= y = let (q,r) = divMod (x-y) y
--                in (q+1,r)
fromDigits :: [Int] -> Int
fromDigits l = aux 0 l
            where aux n [] = n
                  aux n (h:t) = aux ((10*n)+h) t


