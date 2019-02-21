module Cifry where
cifry :: Integer -> [Integer]
cifry x = if (x<10) then [x] else cifry (x `div` 10) ++[ x  `mod` 10 ]        

cifryR :: Integer -> [Integer]
cifryR x= if (x<10) then [x] else [ x  `mod` 10 ] ++ cifryR ( x `div` 10)   
