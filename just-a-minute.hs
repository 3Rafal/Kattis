-- https://open.kattis.com/problems/justaminute

writeOutput :: Double -> String
writeOutput x =
  if x > 1.0 then show x
  else "measurement error"

calc :: [(Int, Int)] -> Double
calc =
  (\(x,y) -> fromIntegral x / fromIntegral y)
  . foldr (\(x , y) (ax, ay) -> (x + ax, y + ay)) (0,0)

readInput :: String -> [(Int, Int)]
readInput = fmap ((\x -> (read (x !! 1), 60 * read (x !! 0))) .  words) . tail . lines

run = writeOutput . calc . readInput

main = interact run
inp = 
    "1\n" ++
    "1 61"

inp2 =
  "3\n" ++
  "5 560\n" ++
  "10 600\n" ++
  "2 264"
