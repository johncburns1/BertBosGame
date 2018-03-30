--Jack Burns
--
--Exercise 1
halve :: [a] -> ([a], [a])

--xs is an empty tuple of lists
halve xs | null xs = ([], [])

         --take n, applied to a list xs, returns the
         --prefix of xs of length n, or xs itself if n > length xs
         --drop n xs returns the suffix of xs after the first n elements

         | length xs `mod` 2 == 0  = (take temp xs, drop temp xs)
         --if the list is of odd length

         | otherwise = (take (temp+1) xs,  drop (temp+1) xs)
         where temp = length xs `div` 2

--Excercise 2
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * (product' xs)

--Exercise 3
safetaila :: Eq a => [a] -> [a]
safetailb :: Eq a => [a] -> [a]
safetailc :: [a] -> [a]

safetaila xs = if xs == [] then [] else tail xs
safetailb xs
          | xs == [] = []
          | otherwise = tail xs

safetailc [] = []
safetailc (_:xs) = xs

--Excercise 4
mult :: Integer -> Integer -> Integer -> Integer
mult = (\z y x -> (z * y * x))

--Excercise 5
replicate' :: Int -> a -> [a]
replicate' n arg = [arg | _ <- [1..n]]
