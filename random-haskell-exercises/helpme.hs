-- https://open.kattis.com/problems/helpme

import Control.Monad (join)
import Data.List (sortBy, intercalate)
import Data.Maybe (mapMaybe)
import Data.Ord (Ordering, Down(..), comparing)

data Piece = K | Q | R | B | N | P
  deriving (Eq, Ord)

instance Show Piece where
  show K = "K"
  show Q = "Q"
  show R = "R"
  show B = "B"
  show N = "N"
  show P = ""

data Color = White | Black
data PlayerPiece = PlayerPiece Color Piece

piece :: Char -> Maybe PlayerPiece
piece 'K' = Just $ PlayerPiece White K
piece 'Q' = Just $ PlayerPiece White Q
piece 'R' = Just $ PlayerPiece White R
piece 'B' = Just $ PlayerPiece White B
piece 'N' = Just $ PlayerPiece White N
piece 'P' = Just $ PlayerPiece White P
piece 'k' = Just $ PlayerPiece Black K
piece 'q' = Just $ PlayerPiece Black Q
piece 'r' = Just $ PlayerPiece Black R
piece 'b' = Just $ PlayerPiece Black B
piece 'n' = Just $ PlayerPiece Black N
piece 'p' = Just $ PlayerPiece Black P
piece _   = Nothing

type File = Int

files = "abcdefgh"

fileToChar :: File -> Char
fileToChar f = files !! (f - 1)

type Rank = Int

getRank :: Rank -> Char
getRank = head . show

type Position = (File, Rank)

filterRank str = map (str !!) [2,6..31]

type ParsedPieces = [(Maybe PlayerPiece, Position)]
type Pieces = [(Piece, Position)]

printPiece :: (Piece, Position) -> String
printPiece (p, (f, r)) = show p ++ [fileToChar f] ++ [getRank r]

piecesToString :: Pieces -> String
piecesToString = intercalate "," . map printPiece

whitePieces :: ParsedPieces -> Pieces
whitePieces = mapMaybe get
  where get (Just (PlayerPiece White p), position) = Just (p, position)
        get _ = Nothing

compareWhites :: (Piece, Position) -> (Piece, Position) -> Ordering 
compareWhites (p1, (f1, r1)) (p2, (f2, r2)) =
  compare p1 p2
  <> compare r1 r2
  <> compare f1 f2

whites :: ParsedPieces -> String
whites = ("White: " ++) . piecesToString . sortBy compareWhites . whitePieces

compareBlacks :: (Piece, Position) -> (Piece, Position) -> Ordering 
compareBlacks (p1, (f1, r1)) (p2, (f2, r2)) =
  compare p1 p2
  <> compare (Down r1) (Down r2)
  <> compare f1 f2

blacks :: ParsedPieces -> String
blacks = ("Black: " ++) . piecesToString . sortBy compareBlacks . blackPieces

result :: ParsedPieces -> String
result pcs = unlines [whites pcs, blacks pcs]

blackPieces :: ParsedPieces -> Pieces
blackPieces = mapMaybe get
  where get (Just (PlayerPiece Black p), position) = Just (p, position)
        get _ = Nothing

parseRank :: Rank -> String -> ParsedPieces
parseRank r =
  fmap (\(x,y) -> (piece y, (x, r)))
  . zip [1..8]

parseBoard :: [String] -> ParsedPieces
parseBoard = join . zipWith parseRank (reverse [1..8])

readInput :: String -> [String]
readInput =
  fmap (filterRank . snd)
  . filter (even . fst)
  . zip [1..] . lines

run = result . parseBoard . readInput

main = interact run
  
inp =
  "+---+---+---+---+---+---+---+---+\n" ++
  "|.r.|:::|.b.|:q:|.k.|:::|.n.|:r:|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|:p:|.p.|:p:|.p.|:p:|.p.|:::|.p.|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|...|:::|.n.|:::|...|:::|...|:p:|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|:::|...|:::|...|:::|...|:::|...|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|...|:::|...|:::|.P.|:::|...|:::|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|:P:|...|:::|...|:::|...|:::|...|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|.P.|:::|.P.|:P:|...|:P:|.P.|:P:|\n" ++
  "+---+---+---+---+---+---+---+---+\n" ++
  "|:R:|.N.|:B:|.Q.|:K:|.B.|:::|.R.|\n" ++
  "+---+---+---+---+---+---+---+---+"

inp2 =
    "+---+---+---+---+---+---+---+---+\n" ++
    "|...|:::|...|:::|...|:::|...|:::|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|:::|...|:::|...|:::|...|:::|...|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|...|:::|...|:::|...|:::|...|:::|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|:::|...|:::|...|:::|...|:::|.k.|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|...|:::|...|:::|...|:::|...|:::|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|:::|...|:::|...|:::|...|:::|...|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|...|:::|...|:::|...|:::|...|:::|\n" ++
    "+---+---+---+---+---+---+---+---+\n" ++
    "|:::|...|:::|...|:k:|...|:::|...|\n" ++
    "+---+---+---+---+---+---+---+---+\n"
