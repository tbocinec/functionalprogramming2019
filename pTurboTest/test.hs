module Cifry where
cifry :: Integer -> [Integer]
cifry x = if (x<10) then [x] else cifry (x `div` 10) ++[ x  `mod` 10 ]        

cifryR :: Integer -> [Integer]
cifryR x= if (x<10) then [x] else [ x  `mod` 10 ] ++ cifryR ( x `div` 10)       

intToList :: Integer -> [Integer]
intToList x = if(x < 10 ) then [x] else intToList (x `div` 10) ++ [x `mod` 10]

 
delitelne11    :: Integer -> Bool
delitelne11 x = (rozdiel (x)) `mod` 11 == 0

rozdiel :: Integer -> Integer
rozdiel x = abs(p-n)  where (p, n) = foldr(\x -> \(n,p) -> (p,n+x)) (0,0) (intToList(x))



rozdielParnychNeparnych x = abs(p-n) where (n,p) = foldr(\x -> \(n,p) -> (p,n+x)) (0,0) x

delitele :: Integer -> [Integer]
delitele n = [i |i <- [1..(n `div` 2)], n `mod` i == 0]

prime :: Integer -> Integer
prime n = if (length (delitele (n))) == 1 then n else 0

primes :: [Integer]
primes = [prime n | n <- [1..]]

priklad =[ [1,7,5,3,2,8,4,9,6],
 [9,4,2,6,7,1,3,8,5],
 [3,6,8,5,9,4,2,7,1],
 [8,2,9,1,3,5,6,4,7],
 [6,5,3,4,8,7,9,1,2],
 [7,1,4,9,6,2,5,3,8],
 [2,3,1,8,4,6,7,5,9],
 [4,8,7,2,5,9,1,6,3],
 [5,9,6,7,1,3,8,2,4]]

sudokuStvorce :: [[Integer]] -> [[Integer]]
sudokuStvorce x = [ [x!!(i+ii)!!(n+nn) |  i <- [0,1,2] , n <-[0,1,2] ] |ii <- [0,3,6],nn <- [0,1,2]]


data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)
maxTree :: (Ord t) => (BTree t) -> t
maxTree (Node Nil t Nil)   = t
maxTree (Node l  t Nil)   = max (maxTree l)  t
maxTree (Node Nil t r)   = max (maxTree r) t
maxTree (Node l  t r)   = max (max (maxTree l)  (maxTree r)) t

minTree :: (Ord t) => (BTree t) -> t
minTree (Node Nil t Nil)   = t
minTree (Node l  t Nil)   = min (minTree l)  t
minTree (Node Nil t r)   = min (minTree r) t
minTree (Node l  t r)   = min (min (minTree l)  (minTree r)) t

minmax :: (Ord t) => (BTree t) -> (t, t)
minmax x = (minTree x,maxTree x)
--main = return ()
--

