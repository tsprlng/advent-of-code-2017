module Day5 where

import qualified Data.Map as M
import Control.Monad (liftM)

type Jumps = M.Map Int Int  -- position => offset

jump1 :: Jumps -> Int -> (Int, Jumps)
jump1 jumps pos = (newPos, M.alter (liftM (+1)) pos jumps)
  where
    newPos = pos + (maybe (error "ran out") id $ M.lookup pos jumps)

jump2 :: Jumps -> Int -> (Int, Jumps)
jump2 jumps pos = (newPos, M.alter (liftM alteration) pos jumps)
  where
    offset = maybe (error "ran out") id $ M.lookup pos jumps
    newPos = pos + offset
    alteration = if offset >= 3 then (flip (-) 1) else ((+) 1)

positions jumpFunc initJumps = map fst positionss
  where
    positionss :: [(Int, Jumps)]
    positionss = (0, initJumps) : zipWith jumpFunc (map snd positionss) (map fst positionss)

loadJumps :: [String] -> Jumps
loadJumps jumpStrings = M.fromList $ zip [0..] $ map read jumpStrings

main = do
  input <- liftM lines $ readFile "Day5.txt"
  let howMany1 = length $ takeWhile (< length input) $ positions jump1 $ loadJumps input
  putStrLn . show $ howMany1
  let howMany2 = length $ takeWhile (< length input) $ positions jump2 $ loadJumps input
  putStrLn . show $ howMany2
