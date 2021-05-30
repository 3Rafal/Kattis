run :: String -> String
run = unlines . reverse . tail . lines

main :: IO ()
main = interact run

inp :: String
inp =
  unlines 
  [ "5",
    "1",
    "2",
    "3",
    "4",
    "5"
  ]
