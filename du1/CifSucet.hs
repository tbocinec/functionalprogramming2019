module CifSucet where

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = toInteger (length [x | x<- [a..b], jCislo x])

cifSucet 0 = 0
cifSucet x = (x `mod` 10) + cifSucet (x `div` 10)

totalcifSucet 0 = 0
totalcifSucet x = if (x < 10) then x else totalcifSucet ( cifSucet x)

jCislo x = if totalcifSucet(x) == 5 then True else False

fak 0 = 1
fak n = n * fak (n - 1)

fak30 = fak 30
