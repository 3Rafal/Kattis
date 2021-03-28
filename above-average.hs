-- https://open.kattis.com/problems/aboveaverage

import Text.Printf

type Case = (Int, [Int])

solve :: Case -> Double
solve (n, grades) =
  (fromIntegral aboveAvg / fromIntegral n) * 100
  where
    aboveAvg = length $ filter (\x -> fromIntegral x > avg) grades
    avg = fromIntegral (sum grades) / fromIntegral  n

processInp :: String -> [Case]
processInp = fmap (toCase . (fmap read . words)) . tail . lines
  where toCase is = (head is, tail is)

out :: [Double] -> String
out = unlines . fmap prt
  where prt = printf "%.3f%%"

run :: String -> String
run = out . fmap solve . processInp

main = interact run

inp =
  unlines
    [ "5",
      "5 50 50 70 80 100",
      "7 100 95 90 80 70 60 50",
      "3 70 90 80",
      "3 70 90 81",
      "9 100 99 98 97 96 95 94 93 91"
    ]
