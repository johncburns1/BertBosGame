--Jack Burns
--Bert Bos Puzzle
--
--The solutions will be displayed as a list of all first row
--permutations of Click/No_Click(s) that will
--solve the puzzle. By this point we all know how the puzzle
--works, so instead of displaying the entire thing,
--my bertsgame function simply displays all first rows that are
--solutions.  I included a length_puzzle test function that
--shows the number of solutions for any given n.

--import lines
import Data.List.Split
import Data.Bool
import Data.Char
import System.IO

--create a data structure to represent the squares color state
data Color = Blue | Red
  deriving(Show, Eq)

--create a data structure to represent the clicks vs no_click
data ClickState = Click | No_Click
  deriving(Show, Eq)

--asks for the color state of any given square
isred :: Color -> Bool
isblue :: Color -> Bool
isred x = x == Red
isblue x = x == Blue

--flip_state function flips the color state of a given square
flip_state :: Color -> Color

flip_state x | isred x = Blue
             | otherwise = Red

--helper function to generate all blue grid
grid :: Int -> a -> [[a]]
grid n = replicate n . replicate n

--generates the actual grid of all blue values
generate_grid :: Int -> [[Color]]
generate_grid n = grid n Blue

--generates list of all possible permutations
clicklists :: Int -> [[ClickState]]
clicklists n = sequence(replicate n [Click, No_Click])

--flip_below flips the elements in the row below the current rown'
flip_below :: [ClickState] -> [Color] -> [Color]
flip_below [] [] = []
flip_below (Click:xs) (y:ys) = [x] ++ flip_below xs ys where x = flip_state y
flip_below (No_Click:xs) (y:ys) = [y] ++ flip_below xs ys

--flip_row function that flips the first 2 elements in a row of the puzzle
flip_row :: [ClickState] -> [Color] -> [Color]
flip_row (Click:xs) (y:ys:yss) = do
      let w = flip_state y
      let z = flip_state ys
      flip_row2 xs (w:z:yss)

--flip_row for No_click type
flip_row (No_Click:xs) (y:ys:yss) = flip_row2 xs (y:ys:yss)

--flip_row2 works with squares after the first element
flip_row2 :: [ClickState] -> [Color] -> [Color]

--flip_row2 basecase for Clicks (flips last 2 elements)
flip_row2 [Click] [x, y] = do
      let a = flip_state y
      let b = flip_state x
      [b] ++ [a]

--flip_row2 for Click
flip_row2 (Click:xs) (y:ys:yss:ysss) = do
      let a = flip_state y
      let b = flip_state ys
      let c = flip_state yss
      [a] ++ flip_row2 xs (b:c:ysss)

--flip_row2 basecase for No_Clicks works with the last 2 elements
flip_row2 (No_Click:xs) (y:ys:yss:ysss) = [y] ++ flip_row2 xs (ys:yss:ysss)

--flip_row2 for No_Clicks
flip_row2 [No_Click] [x, y] = [x] ++ [y]

--red_test asks if all of the squares in a row are red
red_test :: [Color] -> Bool
red_test [] = True
red_test (x:xs) | isred x == True = red_test xs
                | otherwise = False

--generate_clicks
--base case is 2 empty lists
--generates list of clicks for the next row from the colors in the above row
generate_clicks :: [Color] -> [ClickState]
generate_clicks [] = []
generate_clicks (Blue:xs) = [Click] ++ generate_clicks xs
generate_clicks (Red:xs) = [No_Click] ++ generate_clicks xs

--traverse
--traverse recurses down the puzzle to the bottom
--It will then return all of the necessary clicks to complete the puzzle
--base case
traverse_board :: [ClickState] -> [[Color]] -> [ClickState]
traverse_board clicks [x] = do
      let a = flip_row clicks x
      [y | y <- clicks, red_test a == True]

--recursive case
traverse_board firtstrowclicks (x:xs:xss) = do
      let a = flip_row firtstrowclicks x
      let b = flip_below firtstrowclicks xs
      let c = generate_clicks a
      traverse_board c (b:xss)

--bertsgame
--this will tie everything together and solve the puzzle
bertsgame :: Int -> [[ClickState]]
bertsgame n = do
      let a = generate_grid n
      let b = clicklists n
      let c = [x | w <- b, x <- traverse_board w a]
      splitEvery n c

--tester function that outputs the number of solutions
length_puzzle :: Int -> Int
length_puzzle n = length (bertsgame n)
