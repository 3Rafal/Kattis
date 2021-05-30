
main :: IO ()
main = interact run

run :: String -> String
run = show . sum . map numb . tail . lines

numb :: String -> Int
numb str = n ^ p
  where
    n = read $ init str
    p = read [last str]

inp =
  unlines
    [ "2",
      "212",
      "1253"
    ]
