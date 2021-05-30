main :: IO ()
main = interact run

run :: String -> String
run =
  show . length . filter (< 0) . map read . words . (!! 1) .  lines

inp = "3\n5 -10 15\n"

inp' = "5\n-14 -5 -39 -5 -7\n"
