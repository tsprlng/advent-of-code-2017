{-# Language BangPatterns #-}

module Day17 where

import Debug.Trace (trace)

import Data.Maybe (fromJust)
import Data.List (elemIndex, findIndex, findIndices)
import Data.Foldable (foldl')
import Data.Array.IArray (array, IArray, (!), (//), elems, ixmap)
import Data.Array.Unboxed (UArray)

doInsert :: Int -> (Int, (Int, UArray Int Int)) -> Int -> (Int, (Int, UArray Int Int))
doInsert steps (pos, (len, !xs)) val
  | mod val 10000 == 0 = trace (show val) $ (insPos + 1, (len+1, newList))
  | otherwise = (insPos + 1, (len+1, newList))
      where
        newList = (ixmap (0,len) (\i -> if i<=insPos then i else i-1) xs) // [(insPos+1, val)]
        insPos = mod (pos + steps) len

insertStep :: Int -> (Int, Int) -> Int -> (Int, Int)
insertStep steps (pos, len) val
  = (insPos + 1, len+1)
      where
        insPos = mod (pos + steps) len


main = do
    let steps = 382
    let init = array (0,0) [(0,0)]
    let afterInserts = (elems.snd.snd) $ foldl' (doInsert steps) (0,(1,init)) [1..2017]
    putStrLn . show $ head.tail $ dropWhile (/=2017) afterInserts

    -- Too slow to solve the problem naively by actually doing inserts!
    --   let afterInserts2 = (elems.snd.snd) $ foldl' (doInsert steps) (0,(1,init)) [1..50000000]
    --   putStrLn . show $ head.tail $ dropWhile (/=0) afterInserts2

    -- Luckily 0 always remains first in the buffer, so we just need to find the last value to be inserted at [1].

    let inserts = map fst $ scanl (insertStep steps) (0,1) [1..50000000]
    putStrLn . show $ last $ findIndices (==1) inserts
