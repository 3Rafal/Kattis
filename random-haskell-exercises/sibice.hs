run :: String -> String
run s =
  unlines $ map (ans . fits . read) matches
  where
    (header : matches) = lines s
    (_ : l1 : l2 : _) = map read $ words header
    fits x = x^2 <= l1^2 + l2^2
    ans True = "DA"
    ans False = "NE"

main :: IO ()
main = interact run

inp :: String
inp =
  unlines
    [ "5 3 4",
      "3",
      "4",
      "5",
      "6",
      "7"
    ]

inp' :: String
inp' =
  unlines
    [ "2 12 17",
      "21",
      "20"
    ]
