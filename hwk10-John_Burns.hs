--Jack Burns
--
--Exercise 1
--declaring the data type Tree
data Tree = Leaf Int | Node Tree Int Tree

sumtree :: Tree -> Int
sumtree (Leaf n) = n
sumtree (Node t1 n t2) = n + sumtree t1 + sumtree t2

--Exercise 2
--declaration of data type Tree2
data Tree2 = Leaf2 Int | Node2 Tree2 Tree2

balanced :: Tree2 -> Bool
balanced (Leaf2 _) = True
balanced (Node2 left right) = let diff = abs (counter left - counter right) in
    diff <= 1 && balanced left && balanced right

--counter helper function to add up the number of elements in a tree
counter :: Tree2 -> Int
counter (Leaf2 _) = 1
counter (Node2 left right) = counter left + counter right

--Exercise 3
clicklists :: Int -> [[[Char]]]
clicklists n = sequence(replicate n ["click", "noclick"])

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node left n right) = flatten left ++ [n] ++ flatten right

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
