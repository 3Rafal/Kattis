import Data.List

run :: String -> String
run = show . length . nub . map (show . flip mod 42 . read) . words

main :: IO ()
main = interact run

inp :: String
inp =
  unlines $ map show [1 .. 10]
