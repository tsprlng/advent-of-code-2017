module Day14 where

import Day10 (knotHash)
import Numeric (readInt, showIntAtBase, readHex)
import Data.Traversable (mapAccumL)
import Data.Maybe (isJust, fromJust)
import Data.List (nub,sort)

readHash :: String -> Integer
readHash hash = fst . head $ readHex hash

toBits hash = align shown
  where
    align b = (replicate (128 - (length b)) '0') ++ b
    shown = showIntAtBase 2 (\d -> if d==0 then '0' else '1') (readHash hash) ""

gridFor key = map (toBits . knotHash . (\n -> key ++ "-" ++ (show n))) [0..127]

testGrid = ["11010100","01010101","00001010","10101101","01101000","11001001","01000100","11010110"]

regionize :: [[Char]] -> [[Maybe Int]]
regionize grid = foldl (\g (x,y) -> tryCombine g x y) labeled [(x,y) | x <- [0..(width-1)],  y <- [0..(height-1)]]
  where
    labeled = snd $ label grid
    label grid = mapAccumL label' 1 grid
    label' acc row = mapAccumL label'' acc row
    label'' acc '0' = (acc, Nothing)
    label'' acc '1' = (acc+1, Just acc)

    width = length $ head grid
    height = length grid

    neighbours :: [[Maybe Int]] -> Int -> Int -> [Int]
    neighbours grid x y = map fromJust $ filter isJust $ map (\(x,y) -> (grid !! y) !! x) nn
      where
        nn = (if x>0 then [(x-1,y)] else []) ++ (if x<(width-1) then [(x+1,y)] else [])
             ++ (if y>0 then [(x,y-1)] else []) ++ (if y<(height-1) then [(x,y+1)] else [])

    update :: [[Maybe Int]] -> [Int] -> Int -> [[Maybe Int]]
    update grid [] regroup = grid
    update grid needles regroup = map (map replaceIfMatch) grid
      where
        replaceIfMatch Nothing = Nothing
        replaceIfMatch (Just x) = if elem x needles then (Just regroup) else (Just x)

    tryCombine :: [[Maybe Int]] -> Int -> Int -> [[Maybe Int]]
    tryCombine grid x y = maybe grid (\v -> update grid (neighbours grid x y) v) ((grid !!y) !!x)

main = do
  putStrLn . show $ sum $ map (length . filter (=='1')) $ gridFor "xlqgujun"
  putStrLn . show $ length $ nub $ sort $ map fromJust $ filter isJust $ concat $ regionize $ gridFor "xlqgujun"
