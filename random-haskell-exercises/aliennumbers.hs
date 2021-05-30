{-# LANGUAGE RecordWildCards #-}
import Data.List
import Data.Maybe

data Lang = Lang
  { digits :: [Char],
    base :: Int
  }

lang :: String -> Lang
lang str =
  Lang
    { digits = str,
      base = length str
    }

toDecimal :: Lang -> String -> Int
toDecimal Lang{..} str =
  go 0 (reverse str)
  where
    go i (c:cs) = (num c *  base ^ i) + go (i+1) cs
    go _ [] = 0
    num = fromJust . flip elemIndex digits

fromDecimal :: Lang -> Int -> String
fromDecimal l = reverse . unfoldr (unf l)

unf :: Lang -> Int -> Maybe (Char,Int)
unf _ 0 = Nothing
unf Lang{..} i = Just (digits !! d, ds)
  where
    (ds,d) = divMod i base

ex :: [String] -> String
ex (n : f : t : _) =
  fromDecimal (lang t)
  $ toDecimal (lang f) n

label :: [String] -> [String]
label = go 1
  where
    go i (s:ss) = ("Case #" ++ show i ++ ": " ++ s) : go (i+1) ss
    go _ [] = []
    
run :: String -> String
run = unlines . label . fmap (ex . words) . tail . lines

main :: IO ()
main = interact run

inp :: String
inp =
  unlines
    [ "4",
      "9 0123456789 oF8",
      "Foo oF8 0123456789",
      "13 0123456789abcdef 01",
      "CODE O!CDE? A?JM!."
    ]
