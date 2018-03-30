--Jack Burns
--
--Exercise 1
import Data.Char
import Data.List

power :: Integer
power = sumList [n^2 | n <- [1..100]]

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

--Exercise 2
exercise2 :: [(Integer, Integer)]
exercise2 = concat [[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]

--Exercise3
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

--Exercise4
crack :: String -> String
crack xs = encode (-factor) xs
   where
      factor = head (positions (minimum chitab) chitab)
      chitab = [chisqr (rotate n table') table | n <- [0..25]]
      table' = freqs xs

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
   where n = allLetters xs

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e| (o, e) <- zip os es]

allLetters :: String -> Int
allLetters xs = length [x | x <- xs, isAlpha x]

count :: Char -> String -> Int
count char inString = length [c | c <- inString, toLower c == char]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

--the new encode
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--copy the original let2int and int2let that use both uppercase and lowercase lettering
let2int :: Char -> Char -> Int
let2int c letter = ord c - ord letter

int2let :: Int -> Char -> Char
int2let n letter = chr (ord letter + n)

--new shift that uses upper/lowercase lettering
shift :: Int -> Char -> Char
shift n c    | isLower c = int2let (((let2int c 'a') + n) `mod` 26) 'a'
             | isUpper c = int2let (((let2int c 'A') + n) `mod` 26) 'A'
             | otherwise = c
