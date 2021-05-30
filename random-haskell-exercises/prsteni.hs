-- https://open.kattis.com/problems/prsteni

import Data.Ratio

readInput :: String -> [Int]
readInput = fmap read . words . last . lines

solve :: [Int] -> [String]
solve is = fmap (lel . (head is %)) (tail is)
  where lel r =
          show (numerator r) ++ "/" ++ show (denominator r)

run = unlines . solve . readInput
main = interact run

inp = "3\n" ++
      "8 4 2"

inp2 = "4\n" ++
       "12 3 8 4"
