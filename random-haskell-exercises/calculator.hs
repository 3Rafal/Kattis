-- https://open.kattis.com/problems/calculator
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative (some)
import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Printf

term :: Parser Double
term = fromIntegral . read <$> some digit
   <|> parens expr

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

expr :: Parser Double
expr = buildExpressionParser table term
  where table = [ [unary '-' neg]
                , [binary '*' mul, binary '/' dv]
                , [binary '+' add, binary '-' sub]
                ]
        unary c op = Prefix (op <$ char c)
        binary c op = Infix (op <$ char c) AssocLeft
        neg x   = -x
        add x y = x + y
        sub x y = x - y
        mul x y = x * y
        dv x y  = x / y

runLine :: [Char] -> String
runLine = printf "%.2f" . fromRight 0 . parse expr "" . filter (/= ' ')

run = init . unlines . fmap runLine . lines

main = interact run 

inp = init $ unlines
  [ "5 - 3-2"
  , "5- (3-2)"
  , "5 / 3 / 2"
  , "5 / (3 / 2)"
  , "1/3+1"
  , "1/(3+1)"
  , "-4--2"
  , "(1-3)/(5+4)"
  , "2*3*5*7*11*13*17*19"
  , "-5/-5" ]
