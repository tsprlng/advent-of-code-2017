module Day19 where

import Data.Char (isLetter)
import Data.Tuple (swap)
import Data.Maybe (isJust, fromJust)
import Data.Array.IArray (array, (!), elems, bounds)
import Data.Array.Unboxed (UArray)

type Vec2 = (Int, Int)
type Position = Vec2  -- (x, y); (0, 0) is bottom left
type Direction = Vec2  -- (0, -1) is down; (1, 0) is right

type Field = UArray Position Char

findStart :: Field -> (Position, Direction)
findStart f = (pos, (0,-1))
  where
    (_,(_,topLine)) = bounds f
    pos = head $ dropWhile ((/='|') . (!) f) $ zip [0..] (repeat topLine)

followPath :: Field -> (Position, Direction) -> [(Position, Char, Direction)]
followPath f (pos@(x,y), dir@(dx,dy))
  = map fromJust $ takeWhile isJust $ iterate (nextChar f . fromJust) $ Just (pos, f ! pos, dir)

nextChar :: Field -> (Position, Char, Direction) -> Maybe (Position, Char, Direction)
nextChar f (pos@(x,y), _, dir@(dx,dy))
  | charHere == '+' = Just $ if validCharForDir char1 turn1 then (pos1, char1, turn1) else (pos2, char2, turn2)
  | charAhead == ' ' = Nothing
  | otherwise = Just ((x+dx, y+dy), charAhead, dir)
  where
    validCharForDir ' '  _    = False
    validCharForDir '|' (_,0) = False
    validCharForDir '-' (0,_) = False
    validCharForDir  _   _    = True
    charHere = f ! pos
    charAhead = f ! (x+dx, y+dy)
    (turn1, turn2) = ((dy, dx), (0-dy, 0-dx))
    (pos1, pos2) = ((x+dy, y+dx), (x-dy, y-dx))
    (char1, char2) = (f ! pos1, f ! pos2)

linesTo2d :: [String] -> Field
linesTo2d lines = array ((0,0),(length (head lines) - 1, length lines - 1)) $ concatMap indexChars indexedLines
  where
    indexChars (lineIdx, line) = map (\(charIdx, char)-> ((charIdx, lineIdx), char)) $ zip [0..] line
    indexedLines = zip [0..] (reverse lines)

main = do
  input <- linesTo2d . lines <$> readFile "Day19.txt"
  putStrLn $ filter isLetter $ map (\(p,c,d) -> c) $ followPath input (findStart input)
  putStrLn . show $ length $ followPath input (findStart input)
