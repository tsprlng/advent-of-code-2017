module Day5 where

import qualified Data.Map as M

type Jumps = M.Map Int Int  -- position => offset

jump1 :: Jumps -> Int -> (Int, Jumps)
jump1 jumps pos = (newPos, M.alter (fmap (+1)) pos jumps)
  where
    newPos = pos + (maybe (error "ran out") id $ M.lookup pos jumps)

jump2 :: Jumps -> Int -> (Int, Jumps)
jump2 jumps pos = (newPos, M.insert pos alteredOffset jumps)
  where
    offset = maybe (error "ran out") id $ M.lookup pos jumps
    newPos = pos + offset
    alteredOffset = if offset >= 3 then (offset - 1) else (offset + 1)

positions jumpFunc initJumps = map fst positionss
  where
    positionss :: [(Int, Jumps)]
    positionss = (0, initJumps) : zipWith jumpFunc (map snd positionss) (map fst positionss)

loadJumps :: [String] -> Jumps
loadJumps jumpStrings = M.fromList $ zip [0..] $ map read jumpStrings

main = do
  input <- lines <$> readFile "Day5.txt"
  let howMany1 = length $ takeWhile (< length input) $ positions jump1 $ loadJumps input
  putStrLn . show $ howMany1
  let howMany2 = length $ takeWhile (< length input) $ positions jump2 $ loadJumps input
  putStrLn . show $ howMany2
