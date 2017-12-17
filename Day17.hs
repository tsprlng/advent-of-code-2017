{-# Language BangPatterns #-}

module Day17 where

import Debug.Trace (trace)

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Foldable (foldl')
import Data.Array.IArray (array, IArray, (!), elems)
import Data.Array.Unboxed (UArray)

doInsert :: Int -> (Int, (Int, UArray Int Int)) -> Int -> (Int, (Int, UArray Int Int))
doInsert steps (pos, (len, !oldList)) val
  | mod val 10000 == 0 = trace (show val) $ (insPos + 1, (len+1, array (0,len) (zip [0..len] newList)))
  | otherwise = (insPos + 1, (len+1, array (0,len) (zip [0..len] newList)))
      where
        xs :: [Int]
        xs = elems oldList
        newList = take (insPos + 1) xs ++ [val] ++ drop (insPos + 1) xs
        insPos = mod (pos + steps) len

main = do
    let steps = 382
    let init = array (0,0) [(0,0)]
    let afterInserts = (elems.snd.snd) $ foldl' (doInsert steps) (0,(1,init)) [1..2017]
    putStrLn . show $ head.tail $ dropWhile (/=2017) afterInserts
    let afterInserts2 = (elems.snd.snd) $ foldl' (doInsert steps) (0,(1,init)) [1..50000000]
    putStrLn . show $ head.tail $ dropWhile (/=0) afterInserts2
