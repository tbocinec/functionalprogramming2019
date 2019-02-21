module RozdielParnychNeparnych where

rozdielParnychNeparnych    :: [Integer] -> Integer
rozdielParnychNeparnych x = abs(p-n) where (n,p) = foldr(\x -> \(n,p) -> (p,n+x)) (0,0) x
