-- https://open.kattis.com/problems/carrots

run = (!! 1) . words . head . lines

main = interact run

inp1 =
  unlines
    [ "2 1",
      "carrots?",
      "bunnies"
    ]

inp2 =
  unlines
    [ "1 5",
      "sovl problmz"
    ]
