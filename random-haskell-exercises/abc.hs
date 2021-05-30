-- https://open.kattis.com/problems/abc

import Data.List (sort)
type ABC = (Int, Int, Int)

abc :: String -> ABC
abc str = (r 0, r 1, r 2)
  where
    ws = sort $ map read $ words str
    r i = ws !! i

order :: ABC -> String -> String
order (a,b,c) =
  unwords . map (show . num)
  where num 'A' = a
        num 'B' = b
        num 'C' = c
        num _ = undefined

run :: String -> String
run i = order nums (input !! 1) 
  where
    input = lines i
    nums = abc $ head input

main = interact run

inp1 = "1 5 3\nABC"
inp2 = "6 4 2\nCAB"
