--jack Burns
--
--import lines
import Data.Char
import Data.List
import Data.Bool

--Exercise 1

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1     = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

--Exercise 2

rmvdupl :: Eq a => [a] -> [a]
rmvdupl [] = []
rmvdupl (x:xs) | x `elem` xs   = rmvdupl xs
               | otherwise     = x: rmvdupl xs
--Exercise 3
sumpairs :: [Int] -> Int -> [(Int, Int)]
sumpairs [] _ = []
sumpairs xs y = [(x, z) | x <- xs, z <- xs , x + z == y]

--Exercise 4

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

--Exercise 5

dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> x * 10 + xs) 0

--Exercise 6
base2int :: Int -> String -> Int
base2int n = foldl (\acc x -> acc * n + digitToInt x) 0
