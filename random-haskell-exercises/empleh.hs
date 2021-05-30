-- https://open.kattis.com/problems/empleh

import Control.Monad (join)
import Data.Char (toUpper, toLower)
import Data.List (concat, drop, elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, (!), lookup)
import Data.Maybe

data Piece = K | Q | R | B | N | P
  deriving (Show)

piece :: Char -> Piece
piece 'K' = K
piece 'Q' = Q
piece 'R' = R
piece 'B' = B
piece 'N' = N

data Color = White | Black

data PlayerPiece = PlayerPiece Color Piece

type File = Int

files = "abcdefgh"

getFile :: Char -> File
getFile x = fromMaybe undefined index + 1
  where
    index = x `elemIndex` files

type Rank = Int

getRank :: Char -> Rank
getRank = read . return

type Position = (File, Rank)

getPosition :: Color -> String -> (Position, PlayerPiece)
getPosition c [p, file, rank] =
  ((getFile file, getRank rank), PlayerPiece c (piece (toUpper p)))
getPosition c [file, rank] =
  ((getFile file, getRank rank), PlayerPiece c P)
getPosition _ _ = undefined

instance Show PlayerPiece where
  show (PlayerPiece White p) = show p
  show (PlayerPiece Black p) = map toLower $ show p

type Positions = Map Position PlayerPiece

data Square = Empty Color | Occupied Color PlayerPiece

instance Show Square where
  show (Empty White) = "..."
  show (Empty Black) = ":::"
  show (Occupied White p) = "." ++ show p ++ "."
  show (Occupied Black p) = ":" ++ show p ++ ":"

color :: Rank -> File -> Color
color r f = if even $ f + r then Black else White

square :: Positions -> File -> Rank -> Square
square ps r f =
  case Map.lookup (f, r) ps of
            (Just p) -> Occupied c p
            Nothing -> Empty c
  where
    c = color f r

horizontalBar = "+" ++ concat (replicate 8 "---+")

rank :: Positions -> Rank -> String
rank ps r = "|" ++ concat lel
  where lel = fmap ((++ "|") . show . square ps r) [1..8]

board :: Positions -> String
board ps =
  unlines $ reverse $ lel ++ [horizontalBar]
   where lel =concatMap (\x -> [horizontalBar, rank ps x]) [1..8]

readInput :: String -> Map Position PlayerPiece
readInput =
  Map.fromList
    . concatMap (\(c, s) -> fmap (getPosition c) s)
    . zip [White, Black]
    . fmap (filter (/= "") . split . drop 7)
    . lines

run = board . readInput

main = interact run

inp =
  "White: Ke1,Qd1,Ra1,Rh1,Bc1,Bf1,Nb1,a2,c2,d2,f2,g2,h2,a3,e4\n"
    ++ "Black: Ke8,Qd8,Ra8,Rh8,Bc8,Ng8,Nc6,a7,b7,c7,d7,e7,f7,h7,h6"

inp1 =
 "White: \n" ++
 "Black: Kh5,Ke1"

split :: String -> [String]
split [] = [""]
split (c : cs)
  | c == ',' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs
