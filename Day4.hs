module Day4 where

import Data.List (sort, group)

isValid1 :: String -> Bool
isValid1 phrase = all (\l -> length l == 1) $ group . sort $ words phrase

isValid2 :: String -> Bool
isValid2 phrase = all (\l -> length l == 1) $ group . sort $ map sort $ words phrase

main = do
  input <- lines <$> readFile "Day4.txt"
  putStrLn . show $ length $ filter isValid1 input
  putStrLn . show $ length $ filter isValid2 input
