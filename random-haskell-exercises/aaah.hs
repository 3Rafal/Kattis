-- https://open.kattis.com/problems/aaah

solve :: [String] -> String
solve xs =
  if len 0 >= len 1 then "go" else "no"
  where len = length . (xs !!)

run :: String -> String
run = solve . lines
  
main = interact run

inp = "aaah\naaaaah"
inp2 = "aaah\nah"
