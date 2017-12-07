module Day3 where

import qualified Data.Map as M

addV (a,b) (c,d) = (a+c, b+d)

moves = concatMap moves' [0..]
  where
    moves' n = replicate (2*n) (-1,0) ++ replicate (2*n) (0,-1) ++ replicate (2*n + 1) (1,0) ++ replicate (2*n + 1) (0,1)

positions = scanl addV (0,0) moves

distance n = distance' $ positions !! (n-1)
  where
    distance' (x,y) = abs x + abs y


type Memory = M.Map (Integer,Integer) Integer

update :: Memory -> (Integer,Integer) -> (Integer,Memory)
update m pos = (result, M.insert pos result m)
  where
    result :: Integer
    result = sum $ map (\pos2 -> maybe 0 id (M.lookup pos2 m)) $ map (addV pos) [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

updates = map fst updates'
  where
    updates' :: [(Integer, Memory)]
    updates' = scanl inserrt (1, M.insert (0,0) 1 M.empty) $ tail positions
    inserrt :: (Integer, Memory) -> (Integer, Integer) -> (Integer, Memory)
    inserrt lastInsert pos = update (snd lastInsert) pos


main = do
  putStrLn . show $ distance 265149
  putStrLn . show $ head $ dropWhile (<=265149) updates
