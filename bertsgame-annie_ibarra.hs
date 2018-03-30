--
--Annie Ibarra
--

import Data.List.Split
import Data.Char
import Data.Bool
import System.IO

data Color = Blue | Red
  deriving (Show, Eq)

data Click_State = Click | No_Click
  deriving (Show, Eq)

--Returns click state
clickState :: Click_State -> Click_State
clickState x = x

--Returns color
colorState :: Color -> Color
colorState x = x

--Checks if color is red
isRed :: Color -> Bool
isRed x = colorState x == Red

--Checks if color is blue
isBlue :: Color -> Bool
isBlue x = colorState x == Blue

--Flips color state (flip_state)
flipState :: Color -> Color
flipState x | isRed x   = Blue
            | otherwise = Red

--Checks if all squares in a row are red (red_test)
redTest :: [Color] -> Bool
redTest [] = True
redTest (x:xs) | isRed x   = True == redTest xs
               | otherwise = False

--Generates grid to fill with blue values (generate_grid)
grid :: Int -> a -> [[a]]
grid x = replicate x . replicate x

--Generates board grid of blue values (generate_board)
generateGrid :: Int -> [[Color]]
generateGrid x = grid x Blue

--Generates list of permutations (generate_click_perm)
clickLists :: Int -> [[Click_State]]
clickLists x = sequence(replicate x [Click, No_Click])

--Flips first 2 elements in a row (flipRow)
--Both Click and No_Click versions of function
flipRow :: [Click_State] -> [Color] -> [Color]
flipRow (Click:xs) (y:ys:yss) = do
  let a = flipState y
  let b = flipState ys
  flipRow2 xs (a:b:yss)
flipRow (No_Click:xs) (y:ys:yss) =
  flipRow2 xs (y:ys:yss)

--Works with squares after first elements (flipRow2)
flipRow2 :: [Click_State] -> [Color] -> [Color]
flipRow2 [Click] [x,y] = do
  let a = flipState y
  let b = flipState x
  [b] ++ [a]
flipRow2 [No_Click] [x,y] = [x] ++ [y]
flipRow2 (Click:xs) (y:ys:yss:ysss) = do
  let a = flipState y
  let b = flipState ys
  let c = flipState yss
  [a] ++ flipRow2 xs (b:c:ysss)
flipRow2 (No_Click:xs) (y:ys:yss:ysss) =
  [y] ++ flipRow2 xs (ys:yss:ysss)

--Flips elements in the row below current row (flip_next_row)
flipNextRow :: [Click_State] -> [Color] -> [Color]
flipNextRow [] [] = [] --base case
flipNextRow (Click:xs) (y:ys) = [x] ++ flipNextRow xs ys
                              where x = flipState y
flipNextRow (No_Click:xs) (y:ys) = [y] ++ flipNextRow xs ys

--Generates list of clicks for the next row (generate_clicks)
--based on previous row
generateClicks :: [Color] -> [Click_State]
generateClicks [] = [] --base case
generateClicks (Blue:xs) = [Click] ++ generateClicks xs
generateClicks (Red:xs) = [No_Click] ++ generateClicks xs

--Traverse recurses down the puzzle to the bottom (traverse)
--Returns necessary clicks to complete puzzle
traverseBoard :: [Click_State] -> [[Color]] -> [Click_State]
traverseBoard clix [x] = do --base case
  let a = flipRow clix x
  [y | y <- clix, redTest a == True]
traverseBoard clix (x:xs:xss) = do
  let a = flipRow clix x
  let b = flipNextRow clix xs
  let c = generateClicks a
  traverseBoard c (b:xss)

--Ties everything together to solve the puzzle (bertsgame)
--bertsgame :: Int -> [[Click_State]]
bertsgame x = do
  let a = generateGrid x
  let b = clickLists x
  let c = [y | w <- b, y <- traverseBoard w a]
  splitEvery x c

--Tester function for # of solutions
puzzleLength :: Int -> Int
puzzleLength x = length (bertsgame x)
