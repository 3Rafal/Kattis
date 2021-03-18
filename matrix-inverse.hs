-- https://open.kattis.com/problems/matrix

data Matrix = Matrix
  { ma :: Integer,
    mb :: Integer,
    mc :: Integer,
    md :: Integer
  }
  deriving Show

getInverse :: Matrix -> Matrix
getInverse (Matrix a b c d) =
  Matrix (divDet d)
         (divDet (-b))
         (divDet (-c))
         (divDet a)
  where divDet x = x `div` (a * d - b * c)

matrixOut n (Matrix a b c d) =
  "Case " ++ show n ++ ":\n" ++
  show a ++ " " ++ show b ++ "\n" ++
  show c ++ " " ++ show d

getPair :: String -> (Integer, Integer)
getPair str =
  (read $ ws !! 0, read $ ws !! 1)
  where ws = words str

toMatrices :: [String] -> [Matrix]
toMatrices [] = []
toMatrices (up : down : "" : tail) =
  Matrix a b c d : toMatrices tail
  where (a, b) = getPair up
        (c, d) = getPair down

readInput :: String -> [Matrix]
readInput = toMatrices . lines

solve :: [Matrix] -> [Matrix]
solve = map getInverse

writeOutput :: [Matrix] -> String
writeOutput = unlines . zipWith matrixOut [1..]

run = writeOutput . solve . readInput

main :: IO ()
main = interact run

inp = "1 0\n0 1\n\n30 29\n1 1\n\n-7 -16\n4 9\n\n"

