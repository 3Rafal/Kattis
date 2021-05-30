-- https://open.kattis.com/problems/patuljci

import Data.List (permutations, find, sort, subsequences)
import Data.Maybe (fromJust)

readInput :: String -> [Int]
readInput = fmap read . lines

solve :: [Int] -> [Int]
solve is =
  filter (`elem` lel) is
  where lel =
          fromJust
            $ find ((== 100) . sum)
            $ filter ((== (length is - 2)) . length)
            $ subsequences is

writeInput :: [Int] -> String
writeInput = unlines . fmap show
run :: String -> String
run = writeInput . solve . readInput

main = interact run

inp = "7\n8\n10\n13\n15\n19\n20\n23\n25"
inp2 = "8\n6\n5\n1\n37\n30\n28\n22\n36"
