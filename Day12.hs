module Day12 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (sort, nub)

stuffParser :: GenParser Char st (String, [String])

stuffParser = do
  prog <- many1 digit
  string " <-> "
  progs <- sepBy (many1 digit) (string ", ")
  return (prog, progs)

parseStuff = either (error.show) id <$> parse stuffParser ""

listProgs mapp seen prog
  | elem prog seen = []
  | otherwise = prog : concatMap (listProgs mapp (prog:seen)) (fromJust $ M.lookup prog mapp)

findGroup input prog = nub . sort $ listProgs input [] prog

findGroups input = findGroups' input [] (map fst $ M.toList input)
  where
    findGroups' input groupsSoFar [] = groupsSoFar
    findGroups' input groupsSoFar (prog:progs)
      | any (elem prog) groupsSoFar = findGroups' input groupsSoFar progs
      | otherwise = findGroups' input (findGroup input prog : groupsSoFar) progs

-- TODO this has shit time complexity (not that it matters for the given input)

main = do
  input <- M.fromList . map parseStuff . lines <$> readFile "Day12.txt"
  putStrLn . show $ length $ findGroup input "0"
  putStrLn . show $ length $ findGroups input
