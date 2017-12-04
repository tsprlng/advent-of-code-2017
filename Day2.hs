module Day2 where
import Data.List (permutations, tails)
import Control.Monad (liftM)

checksum1 :: [[String]] -> Int
checksum1 lines = sum $ map linesum lines
  where
    linesum vals = maximum vals' - minimum vals'
      where
        vals' = map read vals :: [Int]

checksum2 :: [[String]] -> Int
checksum2 lines = sum $ map linesum lines
  where
    linesum vals = div a b
      where
        vals' = map read vals :: [Int]
        (a,b) = head $ filter (\(a,b) -> a /= b && mod a b == 0) $ possibles
        possibles = [ (x,y) | (x:rest) <- tails (vals' ++ reverse vals') , y <- rest ]

main = do
  input <- liftM (map words . lines) $ readFile "Day2.txt"
  putStrLn . show $ checksum1 input
  putStrLn . show $ checksum2 input
