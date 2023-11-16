--RECURSION SOBRE LISTAS

--PUNTO 1

--1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

