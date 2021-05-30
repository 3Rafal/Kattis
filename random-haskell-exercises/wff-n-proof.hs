-- https://open.kattis.com/problems/wffnproof

import Data.Maybe

data Leaf = P | Q | R | S | T

instance Show Leaf where
  show P = "p"
  show Q = "q"
  show R = "r"
  show S = "s"
  show T = "t"

data SNode = N
  deriving Show

data DNode = K | A | C | E
  deriving Show

data WFF
  = L Leaf
  | SN SNode WFF
  | DN DNode WFF WFF
  | NoWFF

instance Show WFF where
  show (L x) = show x
  show (SN n wff) = show n ++ show wff
  show (DN n wff1 wff2) = show n ++ show wff1 ++ show wff2
  show NoWFF = "no WFF possible"

leafs :: String -> [Leaf]
leafs = mapMaybe f
  where f 'p' = Just P
        f 'q' = Just Q
        f 'r' = Just R
        f 's' = Just S
        f 't' = Just T
        f _   = Nothing

sNodes :: String -> [SNode]
sNodes = mapMaybe f
  where f 'N' = Just N
        f _   = Nothing

nestSingles :: Leaf -> [SNode] -> WFF
nestSingles l = foldr SN (L l)

dNodes :: String -> [DNode]
dNodes = mapMaybe f
  where f 'K' = Just K
        f 'A' = Just A
        f 'C' = Just C
        f 'E' = Just E
        f _   = Nothing

nestDoubles :: WFF -> [Leaf] -> [DNode] -> WFF
nestDoubles base ls dns =
  foldr link base $ zip ls dns
  where link (leaf, node) wff = DN node (L leaf) wff

buildTree :: String -> WFF
buildTree str =
  if null ls then NoWFF
  else nd
  where
    nd = nestDoubles ns tl dns
    ns = nestSingles hl sns
    hl = head ls
    tl = tail ls
    ls  = leafs str
    sns = sNodes str
    dns = dNodes str

solve :: String -> String
solve = show . buildTree

readInput :: String -> [String]
readInput = init . lines

writeOutput = unlines

run :: String -> String
run = writeOutput . map solve . readInput

main :: IO ()
main = interact run 
