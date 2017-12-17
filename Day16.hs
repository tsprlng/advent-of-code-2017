module Day16 where

import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import Data.Foldable (foldl')
import Data.Array.IArray (array, IArray, (!), elems)
import Data.Array.Unboxed (UArray)

data DanceMove = Spin Int | SwapPos Int Int | SwapProgs Char Char
  deriving (Eq, Show)

stuffParser :: GenParser Char st [DanceMove]
stuffParser = sepBy moveParser (string ",")
moveParser = choice [spin, swapPos, swapProgs]
  where
    spin = do
      string "s"
      Spin . read <$> many1 digit
    swapPos = do
      string "x"
      a <- read <$> many1 digit
      string "/"
      b <- read <$> many1 digit
      return $ SwapPos a b
    swapProgs = do
      string "p"
      a <- count 1 (noneOf "/")
      string "/"
      b <- count 1 (noneOf "/")
      return $ SwapProgs (head a) (head b)

parseStuff = either (error.show) id <$> parse stuffParser ""

doMove :: [Char] -> DanceMove -> [Char]
doMove ps (Spin un_n) = let n = (length ps) - un_n in drop n ps ++ take n ps
doMove ps (SwapProgs a b) = doMove ps (SwapPos (fromJust $ elemIndex a ps) (fromJust $ elemIndex b ps))
doMove ps (SwapPos a b)
  = take (min a b) ps ++ [ps !! max a b]
      ++ (take (max a b - min a b - 1) $ drop (min a b + 1) ps)
      ++ [ps !! min a b] ++ (drop (max a b + 1) ps)

danceAgain :: UArray Char Char -> UArray Char Char -> UArray Char Char
danceAgain reordering order = array ('a','p') $ zip ['a'..'p'] $ map ((!) reordering) $! elems order

iterate' f x = x : iterate' f (f x)

main = do
    input <- parseStuff <$> readFile "Day16.txt"
    let oneDance = foldl doMove ['a'..'p'] input
    putStrLn oneDance
    let reordering = array ('a','p') $ zip ['a'..'p'] oneDance
    let initial = array ('a','p') $ zip ['a'..'p'] ['a'..'p'] :: UArray Char Char
    putStrLn . elems $ foldl' (\pos meh -> danceAgain reordering pos) initial (replicate 1000000000 "meh")
