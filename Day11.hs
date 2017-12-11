module Day11 where

import Data.Char (isSpace)

splitCommas :: String -> [String]
splitCommas = split' ""
  where
    split' accum [] = [accum]
    split' accum (',':str) = accum : split' "" str
    split' accum (c:str) = split' (accum ++ [c]) str
      -- TODO this is STILL clearly silly

move (nw, n, ne, se, s, sw) "n"
  | s >= 1 = (nw, n, ne, se, s-1, sw)
  | sw >= 1 = (nw+1, n, ne, se, s, sw-1)
  | se >= 1 = (nw, n, ne+1, se-1, s, sw)
  | otherwise = (nw, n+1, ne, se, s, sw)

move (nw, n, ne, se, s, sw) "s"
  | n >= 1 = (nw, n-1, ne, se, s, sw)
  | nw >= 1 = (nw-1, n, ne, se, s, sw+1)
  | ne >= 1 = (nw, n, ne-1, se+1, s, sw)
  | otherwise = (nw, n, ne, se, s+1, sw)

move (nw, n, ne, se, s, sw) "sw"
  | ne >= 1 = (nw, n, ne-1, se, s, sw)
  | se >= 1 = (nw, n, ne, se-1, s+1, sw)
  | n  >= 1 = (nw+1, n-1, ne, se, s, sw)
  | otherwise = (nw, n, ne, se, s, sw+1)

move (nw, n, ne, se, s, sw) "se"
  | nw >= 1 = (nw-1, n, ne, se, s, sw)
  | sw >= 1 = (nw, n, ne, se, s+1, sw-1)
  | n  >= 1 = (nw, n-1, ne+1, se, s, sw)
  | otherwise = (nw, n, ne, se+1, s, sw)

move (nw, n, ne, se, s, sw) "nw"
  | se >= 1 = (nw, n, ne, se-1, s, sw)
  | ne >= 1 = (nw, n+1, ne-1, se, s, sw)
  | s  >= 1 = (nw, n, ne, se, s-1, sw+1)
  | otherwise = (nw+1, n, ne, se, s, sw)

move (nw, n, ne, se, s, sw) "ne"
  | sw >= 1 = (nw, n, ne, se, s, sw-1)
  | nw >= 1 = (nw-1, n+1, ne, se, s, sw)
  | s  >= 1 = (nw, n, ne, se+1, s-1, sw)
  | otherwise = (nw, n, ne+1, se, s, sw)

-- TODO generalize?

doMoves = foldl move (0,0,0,0,0,0)
eachPosition = scanl move (0,0,0,0,0,0)

howFar (a,b,c,d,e,f) = a + b + c + d + e + f

main = do
  input <- splitCommas . filter (not.isSpace) <$> readFile "Day11.txt"
  putStrLn . show $ howFar $ doMoves input
  putStrLn . show $ fst . maximum . map (\x->(howFar x, x)) $ eachPosition input
