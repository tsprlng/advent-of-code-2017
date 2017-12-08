module Day1 where
import Data.Char (digitToInt, isDigit)

valueIfMatching :: Char -> Char -> Int
valueIfMatching a b =
  if a == b then digitToInt a else 0

count1 input =
  sum $ zipWith valueIfMatching input (tail input ++ [head input])

count2 input =
  sum $ zipWith valueIfMatching input (end ++ start)
  where
    (start, end) = splitAt (length input `div` 2) input

main = do
  input <- filter isDigit <$> readFile "Day1.txt"
  putStrLn . show $ count1 input
  putStrLn . show $ count2 input
