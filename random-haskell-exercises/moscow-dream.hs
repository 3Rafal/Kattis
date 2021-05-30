-- https://open.kattis.com/problems/moscowdream

data PreparedProblems = PreparedProblems
  { easy :: Integer,
    medium :: Integer,
    hard :: Integer
  }

data Result = YES | NO deriving Show

toPreparedProblems :: [String] -> PreparedProblems
toPreparedProblems s = PreparedProblems
  { easy   = read $ s !! 0,
    medium = read $ s !! 1,
    hard   = read $ s !! 2
  }

isAnyZero pps = easy pps == 0 || medium pps == 0 || hard pps == 0

sumPs pps = easy pps + medium pps + hard pps

maxAvailable pps =
  if isAnyZero pps then 0
  else sumPs pps

newtype PSetCount = PSetCount Integer
toPSCount = PSetCount . read . (!! 3)

readInput :: [Char] -> (PreparedProblems, PSetCount)
readInput inp =
  (toPreparedProblems ws, toPSCount ws)
  where ws = words inp

solve (pps, PSetCount c) = 
  if maxAvailable pps >= c && c >= 3 then YES else NO

writeOutput = show

run = writeOutput . solve . readInput

main :: IO ()
main = interact run 
