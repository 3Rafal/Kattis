-- https://open.kattis.com/problems/tri

data Op = Plus | Minus | Div | Multi

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Div = "/"
  show Multi = "*"

data Or = Pre | Post
data Result = Result Op Or

ops = [Plus, Minus, Div, Multi]

getOp (i1, i2, i3) 
  | i1 + i2 == i3 = Result Plus Post
  | i1 - i2 == i3 = Result Minus Post
  | i1 `div` i2 == i3 = Result Div Post
  | i1 * i2 == i3 = Result Multi Post
  | i2 + i3 == i1 = Result Plus Pre
  | i2 - i3 == i1 = Result Minus Pre
  | i2 `div` i3 == i1 = Result Div Pre
  | i2 * i3 == i1 = Result Multi Pre
  | otherwise = undefined

result :: Result -> (Int, Int, Int) -> String
result (Result op Post) (i1, i2, i3) = show i1 ++ show op ++ show i2 ++ "=" ++ show i3
result (Result op Pre) (i1, i2, i3) = show i1 ++ "=" ++ show i2 ++ show op ++ show i3

readInput :: String -> (Int, Int, Int)
readInput = (\x -> (head x, x !! 1, x !! 2)) . map read . words

run :: String -> String
run str =
  result (getOp i) i
  where i = readInput str

main = interact run

inp  = "5 3 8"
inp2 = "5 15 3"
