module Day6 where
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Control.Monad (liftM)

rotate n xs = zipWith const (drop n (cycle xs)) xs

redist dist = map newBlocks $ zip3 [0..] (rotate (leng-chosen-1) [1..leng]) dist
  where
    newBlocks x@(idx, over, oldBlocks) = {-trace x $-} evens + (if idx==chosen then 0 else oldBlocks) + (if over<=left then 1 else 0)
    evens = div pile leng
    leng = length dist
    pile = maximum dist
    left = mod pile leng
    chosen = fromJust $ elemIndex pile dist
    --trace' = trace ("evens " ++ show evens ++ "  leng " ++ show leng ++ "  pile " ++ show pile ++ "  left " ++ show left ++ "  chosen " ++ show chosen)

answer1 input = ans [] input
  where
    ans pastDists currentDist
      | elem currentDist pastDists = 0
      | otherwise = 1 + ans (currentDist:pastDists) (redist currentDist)

answer2 input = ans [] input
  where
    ans pastDists currentDist = maybe next (1+) $ elemIndex currentDist pastDists
      where
        next = ans (currentDist:pastDists) (redist currentDist)

main = do
  input <- liftM (map read . concatMap words . lines) $ readFile "Day6.txt" :: IO [Int]
  putStrLn . show $ answer1 input
  putStrLn . show $ answer2 input
