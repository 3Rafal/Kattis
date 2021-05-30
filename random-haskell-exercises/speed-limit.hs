-- https://open.kattis.com/problems/speedlimit

import Data.Bifunctor (Bifunctor(first))

data LogEntry = LogEntry
  { speed :: Integer,
    hours :: Integer
  }
  deriving Show

dummyEntry = LogEntry 0 0

solve :: [LogEntry] -> String
solve es = toStr $ sum $ zipWith dist (dummyEntry : es) es
  where dist prev curr =
         speed curr * (hours curr - hours prev)
        toStr result = show result ++ " miles"

toLogEntry :: String -> LogEntry
toLogEntry x = LogEntry {speed = read $ head ws
                         , hours = read $ ws !! 1}
  where ws = words x 


readInput :: String -> [[LogEntry]]
readInput = mySplt . lines

writeOutput = unlines

isNotSingleEntry :: String -> Bool
isNotSingleEntry = (/= 1) . length . words

getGrouping :: ([[LogEntry]], [String]) -> ([[LogEntry]], [String])
getGrouping (ls, []) = (ls, [])
getGrouping (ls, strs) = getGrouping $ first ((: ls) . map toLogEntry) $ span isNotSingleEntry $ tail strs


mySplt :: [String] -> [[LogEntry]]
mySplt strs = filter (not . null) $ reverse $ fst $ getGrouping ([], strs) 

main :: IO ()
main = interact $ writeOutput . map solve . readInput

testMain = writeOutput . map solve . readInput
inp = "3\n20 2\n30 6\n10 7\n2\n60 1\n30 5\n4\n15 1\n25 2\n30 3\n10 5\n-1"

singList = ["3", "20 2", "30 6", "10 7", "-1"]
singList' = ["20 2", "30 6", "10 7", "-1"]

