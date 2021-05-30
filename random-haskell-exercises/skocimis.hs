import Data.List

main :: IO ()
main = interact run

run :: String -> String
run = show . solve . map read . words

solve :: [Int] -> Int
solve xs =
  let (x:y:z:_) = sort xs in
    max (len x y) (len y z)
  where
    len a b = length [a..b] - 2

inp :: String
inp = "2 3 5"

inp' :: String
inp' = "3 5 9"
