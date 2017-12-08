module Main where
import Data.List (elemIndex)
import Data.Maybe (fromJust)

rotate n xs = end ++ start
  where
    (start, end) = splitAt n xs

findMaximum xs = findMax (-1,0) 0 xs
  where
    findMax ans _ [] = ans
    findMax ans@(lastPos, bestSoFar) myPos (x:xs) = findMax (if x > bestSoFar then (myPos, x) else ans) (myPos+1) xs

redist dist = map newBlocks $ zip3 [0..] (rotate (len-position-1) [1..len]) dist
  where
    len = length dist
    newBlocks x@(idx, order, oldBlocks) = evens + (if idx==position then 0 else oldBlocks) + (if order<=leftOver then 1 else 0)
    (evens, leftOver) = divMod toRedist len
    (position, toRedist) = findMaximum dist

answer1 input = ans [] input  -- how long until pattern starts repeating?
  where
    ans pastDists currentDist
      | elem currentDist pastDists = 0
      | otherwise = 1 + ans (currentDist:pastDists) (redist currentDist)

answer2 input = ans [] input  -- how long is loop?
  where
    ans pastDists currentDist = maybe next (1+) $ elemIndex currentDist pastDists
      where
        next = ans (currentDist:pastDists) (redist currentDist)

main = do
  input <- (map read . concatMap words . lines) <$> readFile "Day6.txt" :: IO [Int]
  putStrLn . show $ answer1 input
  putStrLn . show $ answer2 input
