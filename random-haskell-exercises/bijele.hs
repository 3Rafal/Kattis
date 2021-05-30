run :: String -> String
run =
  unlines
    . map show
    . zipWith (-) expected
    . map read
    . words
  where
    expected = [1, 1, 2, 2, 2, 8]

main :: IO ()
main = interact run

inp :: String
inp = "0 1 2 2 2 7"

inp' :: String
inp' = "2 1 2 1 2 1"
