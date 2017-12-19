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

-- TODO go back to whatever I did in the past that was cleaner than iterate and Maybe

nextChar :: Field -> (Position, Char, Direction) -> Maybe (Position, Char, Direction)
nextChar f (here@(x,y), _, dir@(dx,dy))
  | f ! here == '+'
      = Just $ if wayIsClear char1 way1 then (pos1, char1, way1) else (pos2, char2, way2)
          -- Mercifully there aren't any letters on corners -- always '+', so this is good enough.
  | f ! ahead == ' ' = Nothing
          -- The end of the path since it wasn't a corner.
  | otherwise = Just (ahead, f ! ahead, dir)
  where
    wayIsClear ' '  _    = False
    wayIsClear '|' (_,0) = False
    wayIsClear '-' (0,_) = False
    wayIsClear  _   _    = True
    ahead = (x+dx, y+dy)
    (way1, pos1, char1) = (( dy,  dx), (x+dy, y+dx), f ! pos1)
    (way2, pos2, char2) = ((-dy, -dx), (x-dy, y-dx), f ! pos2)

linesTo2d :: [String] -> Field
linesTo2d lines = array ((0,0),(length (head lines) - 1, length lines - 1)) $ concatMap indexChars indexedLines
  where
    indexChars (lineIdx, line) = map (\(charIdx, char)-> ((charIdx, lineIdx), char)) $ zip [0..] line
    indexedLines = zip [0..] (reverse lines)

main = do
  input <- linesTo2d . lines <$> readFile "Day19.txt"
  putStrLn $ filter isLetter $ map (\(p,c,d) -> c) $ followPath input (findStart input)
  putStrLn . show $ length $ followPath input (findStart input)
