module BiLandia where

pocetMensich::Integer->Integer
pocetMensich 0 = 0
pocetMensich 1 = 0
pocetMensich x = pocetMensichPom x 1 0

pocetMensichPom :: Integer -> Integer -> Integer -> Integer
pocetMensichPom x y z = if x > y then (pocetMensichPom x (y*2) (z+1)) else z

pocetMoznosti::Integer->Integer
pocetMoznosti 0 = 1
pocetMoznosti 1 = 1
pocetMoznosti 2 = 2
pocetMoznosti x = pocetMoznosti(n-1) + pocetMoznosti(n `div` 2) where n = makeEven x

makeEven :: Integer -> Integer
makeEven x = if odd x then x-1 else x

daSa::Integer->Bool
daSa x = True 
