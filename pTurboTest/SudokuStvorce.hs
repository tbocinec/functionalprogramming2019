module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce x = [ [x!!(i+ii)!!(n+nn) |  i <- [0,1,2] , n <-[0,1,2] ] |ii <- [0,3,6],nn <- [0,1,2]]
