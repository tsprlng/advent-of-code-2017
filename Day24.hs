module Day24 where

import Text.Regex.PCRE.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)

eachSelection :: [a] -> [([a],a,[a])]
eachSelection [] = []
eachSelection (x:xs) = (([],x,xs) :) $ map (\(more,c,cs)-> (x:more, c, cs)) $ eachSelection xs

maxStrength :: Int -> [(Int,Int)] -> Int
maxStrength connectTo ports = maximum $ map strength $ eachSelection ports
  where
    strength (xs, (a,b), xxs)
      | a == connectTo = a + b + maxStrength b (xs++xxs)
      | b == connectTo = a + b + maxStrength a (xs++xxs)
      | otherwise = 0

maxLengthAndStrength :: Int -> [(Int,Int)] -> (Int,Int)
maxLengthAndStrength connectTo ports = maximum $ map strength $ eachSelection ports
  where
    strength (xs, (a,b), xxs)
      | a == connectTo = (\(lng,str)-> (lng+1,str+a+b)) $ maxLengthAndStrength b (xs++xxs)
      | b == connectTo = (\(lng,str)-> (lng+1,str+a+b)) $ maxLengthAndStrength a (xs++xxs)
      | otherwise = (0,0)

parseLine :: String -> (Int,Int)
parseLine line = (read a, read b)
  where
    (_,_,_,[a,b]) = fromJust $ either (error.show) id $ unsafePerformIO $ regexec lineParser line
    lineParser = either (error.show) id $ unsafePerformIO $ compile compUTF8 0 "(\\d+)/(\\d+)"

main = do
  input <- map parseLine . lines <$> readFile "Day24.txt"
  putStrLn . show $ maxStrength 0 input
  putStrLn . show . snd $ maxLengthAndStrength 0 input
