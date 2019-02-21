module Del11 where

rozdiel :: Integer -> Integer
rozdiel x = abs(p-n)  where (p, n) = foldr(\x -> \(n,p) -> (p,n+x)) (0,0) (intToList(x))

intToList :: Integer -> [Integer]
intToList x = if(x < 10 ) then [x] else intToList (x `div` 10) ++ [x `mod` 10]

delitelne11    :: Integer -> Bool
delitelne11 x = (rozdiel (x)) `mod` 11 == 0


