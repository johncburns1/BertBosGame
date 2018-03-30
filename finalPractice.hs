myLast :: [a] -> a
myLast [x] = x
myLast [] = error "No end for empty lists"
myLast (_:xs) = myLast xs

myLast1 :: [a] -> a
myLast1 xs = xs !! (length (xs) - 1)

myButLast :: [a] -> a
myButLast [x, y] = x
myButLast [] = error "Not a valid entry"
myButLast (x:xs) = myButLast xs

elementAt :: Int -> [a] -> a
elementAt n xs = xs !! (n-1)

myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myLength1 :: [a] -> Int
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs | reverse xs == xs = True
                | otherwise = False

isPalindrome' xs = if reverse xs == xs then True else False

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

flatten'' :: NestedList a -> [a]
flatten'' (Elem x) = return x
flatten'' (List x) = flatten'' =<< x

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x:(compress (dropWhile (==x) xs))

compress' :: (Eq a) => [a] -> [a]
compress' = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
                | x == head acc = acc
                | otherwise = x:acc

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

dupli :: [a] -> [a]
dupli xs = concat [[x, x] | x <- xs]

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:dupli xs

dupli'' :: [a] -> [a]
dupli'' = foldr (\x acc -> x:x:acc) []

dupli3 :: [a] -> [a]
dupli3 = foldl (\acc x -> acc ++ [x, x]) []

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x:helper xs (k-1)

--data Maybe a = Nothing | Just a
dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n | n == 0 = xs
                | otherwise = ([ i | (i,c) <- ( zip xs [1,2..]), (mod c n) /= 0])

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) (take k xs)

slice' xs i k | i > k = []
              | otherwise = (take (k-i+1) (drop (i-1) xs))

slice'' xs i k = [x | (x, y) <- zip xs [1..k], i <= y]

greatest :: (Num a, Ord a) => [a] -> a
greatest = foldr(\x y -> max x y) 0

shit xs = foldr (h) (0, 0) xs
    where
      h (x, y) (w, z) = ((x+w), (y+z))

shit1 :: (Num a, Ord a) => [a] -> [a] -> [[Char]]
shit1 xs ys = [i | (x, y) <- zip xs ys, i <- if x < y then ["xshit"] else if x > y then ["yshit"] else ["zshit"]]

shit2 :: (Float, Float, Float) -> Maybe String
shit2 (x, y, z) | x == 1.0 && y == 0.0 && z == 0.0 = Just "red"
                | x == 1.0 && y == 1.0 && z == 0.0 = Just "yellow"
                | x == 0.0 && y == 0.0 && z == 1.0 = Just "green"
                | otherwise = Nothing

shit3 (x, y, z) = if x == 1.0 && y == 0.0 && z == 0.0 then Just "red" else if x == 1.0 && y == 1.0 && z == 0.0 then Just "yellow" else if x == 0.0 && y == 0.0 && z == 1.0 then Just "green" else Nothing
