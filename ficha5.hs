import Data.List

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau a [] = []
selgrau a t = filter (\(c,e) -> e==a) t

any1 :: (a -> Bool) -> [a] -> Bool
any1 f [] = False
any1 f (h:t)
 | f (h) = True
 | otherwise = any1 f t

all1 :: (a-> Bool) -> [a] -> Bool
all1 f [] = True
all1 f (x:x1)
 | f x = False
 | otherwise = all1 f x1



conta :: Int -> Polinomio -> Int
conta a [] = 0
conta a t = length (filter (\(c,e) -> a==e) t)

mult :: Monomio -> Polinomio -> Polinomio
mult (x,x1) t = map (\(c,e) -> (x*c,e*x1)) t

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = aux 0 0 l
         where aux a b [] = a
               aux a b (x:x1) =
                 let e = b+x
                 in if e>a then aux e e x1 else aux a e x1

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = aux 1 0 n
  where aux a b 1 = a
        aux a b n =
           let c = a+b
           in aux c a (n-1)

type Mat a = [[a]]

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:x1) = if aux (x:x1) == True then x:nub1 x1 else nub1 x1
           where aux :: Eq a => [a] -> Bool
                 aux [a] = True
                 aux (x:x1:x2) = if x == x1 then False else aux (x:x2) 



