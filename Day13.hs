module Day13 where

import Data.Char (isSpace)

type Scanners = [(Int, Int)]  -- depth, range

positions :: Scanners -> Int -> [Int]
positions scanners offset = map position scanners
  where
    position (depth, range) = mod (depth + offset) (range * 2 - 2)

severity scanners = sum $ zipWith cost scanners $ positions scanners 0
  where
    cost (depth, range) position
      | position == 0 = depth * range
      | otherwise = 0

split :: String -> (Int, Int)
split str = (read a, read b)
  where
    (a, b) = splitt "" (filter (not.isSpace) str)
    splitt a (':':xs) = (a, xs)
    splitt a (x:xs) = splitt (a++[x]) xs

main = do
  input <- map split . lines <$> readFile "Day13.txt"
  putStrLn . show $ severity input
  putStrLn . show $ head $ filter (all (/=0) . positions input) [0..]
