module Primes where

delitele :: Integer -> [Integer]
delitele n = [i | i <- [1..(n `div` 2)], n `mod` i == 0]

prime :: Integer -> Integer
prime n = if (length (delitele (n))) == 1 then n else 0

primes :: [Integer]
primes = [prime n | n <- [1..]]
