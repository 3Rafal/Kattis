import Data.List
import Data.Ord

main :: IO ()
main = interact run

run :: String -> String
run = maxIndex . map (sum . map read . words) . lines

maxIndex :: [Int] -> String
maxIndex = print . maximumBy (comparing snd) . zip [1 ..]
  where
    print (x, y) = show x ++ " " ++ show y

inp =
  unlines
    [ "5 4 4 5",
      "5 4 4 4",
      "5 5 4 4",
      "5 5 5 4",
      "4 4 4 5"
    ]

inp' =
  unlines
    [ "4 4 3 3",
      "5 4 3 5",
      "5 5 2 4",
      "5 5 5 1",
      "4 4 4 4"
    ]
