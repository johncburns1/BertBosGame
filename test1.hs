--dbl using foldr
dbl :: (Num a) => [a] -> [a]
dbl xs = foldr(\x acc -> (x+x:acc)) [] xs

--dbl using foldl
dbl''' :: (Num a) => [a] -> [a]
dbl''' xs = foldl( \a x -> (a ++ [x*2])) [] xs

--using foldr but with point free notation
ddbl :: (Num a) => [a] -> [a]
ddbl = foldr(\x acc -> (x*2:acc)) []

--dbl using map
dbl' :: (Num a) => [a] -> [a]
dbl' xs = map(\x -> (x+x)) xs

--dbl using list comprehension
dbl'' :: (Num a) => [a] -> [a]
dbl'' xs = [x + x | x <- xs]

mystery :: (Num a) => [a] -> [a]
mystery xs = foldr(\x y -> (x:x:y)) [] [y | y <- xs]

--mystery' :: (Num a) => [a] -> [a]
--mystery' xs = foldl(\x acc -> (acc ++ [x, x])) [] xs

reverse' :: [a] -> [a]
reverse' xs = doReverse xs []

doReverse [] ys = ys
doReverse (x:xs) ys = doReverse xs (x:ys)

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (q,v) <- t, k == q]
