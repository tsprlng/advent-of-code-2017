module Day10 where

import Data.Char (ord, isSpace)
import Data.Bits (xor)
import Numeric (showHex)

trace a b = b

splitCommas :: String -> [String]
splitCommas = split' ""
  where
    split' accum [] = [accum]
    split' accum (',':str) = accum : split' "" str
    split' accum (c:str) = split' (accum ++ [c]) str
      -- TODO this is clearly silly

chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
  where
    (as, bs) = splitAt n xs

initialList = [0..255]

knot :: Int -> Int -> Int -> [Int] -> [Int]
knot listLen pos len xs = trace ("knot " ++ show (pos, len, xs)) $ zipWith const restored xs
  where
    restored = d ++ c
    (c,d) = splitAt (listLen - pos) knotted
    knotted = take listLen $ (reverse $ take len b) ++ drop len b
    (a,b) = splitAt pos $ cycle xs

doKnots _       _   _    xs [] = xs
doKnots listLen pos skip xs (len:lens)
  = doKnots listLen (mod (pos + len + skip) listLen) (skip + 1) (knot listLen pos len xs) lens

denseHash xs = map (foldl1 xor) $ chunksOf 16 xs

present ints = concatMap (flip showHex "") ints

knotHash str = present $ denseHash $ doKnots (length initialList) 0 0 initialList mangledStr
  where
    mangledStr = concat $ replicate 64 $ map ord str ++ [17,31,73,47,23]

main = do
  input <- readFile "Day10.txt" :: IO String
  let normalInput = map read $ splitCommas $ input :: [Int]
  putStrLn . show $ (\(a:b:xs) -> a*b) $ doKnots (length initialList) 0 0 initialList normalInput
  putStrLn $ knotHash $ filter (not.isSpace) input
