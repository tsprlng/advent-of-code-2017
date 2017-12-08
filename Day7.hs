module Day7 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (either)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.List (find, group, sort, groupBy, sortOn)
import Control.Exception (throw)


type Name = String
type Weight = Int
type Program = (Name, Weight, [Name])
type Prog = (Name, Weight, Maybe Name, [Name])

type Collector = M.Map Name Prog

lineParser :: GenParser Char st Program
lineParser =
  do name <- many (noneOf " ")
     weight <- string " (" >> many digit >>= \w -> string ")" >> return w
     children <- (string " -> " >> sepBy (many $ noneOf ",") (string ", ")) <|> (return [])
     return (name, read weight, children)

parseLine :: String -> Either ParseError Program
parseLine = parse lineParser ""

absorbErrors :: [Either ParseError Program] -> [Program]
absorbErrors = map (either (error.show) id)


toTree :: [Program] -> (Prog, Collector)
toTree inputs = (root, finalMap)
  where
    root = fromJust $ find (\(_,_,parent,_)-> isNothing parent) $ map snd $ M.toList finalMap
    finalMap = foldl update M.empty inputs
      where
        update :: Collector -> (Name, Weight, [Name]) -> Collector
        update map (name, weight, children) = foldl updateChild (updateSelf map) children
          where
            updateSelf :: Collector -> Collector
            updateSelf = M.alter ((\(_,_,p,_)-> Just (name, weight, p, children)) . entryOrDefault name) name
            updateChild :: Collector -> Name -> Collector
            updateChild map nm = M.alter ((\(n,w,_,cs)-> Just (n, w, Just name, cs)) . entryOrDefault name) nm map
            entryOrDefault name = fromMaybe (name, -1, Nothing, [])


type Answer2 = (Weight, Maybe [(Weight,Prog)])  -- total weight, maybe disagreeing nodes and their subtree weights

findWrong :: (Prog, Collector) -> Answer2
findWrong (root@(_,weight,_,children), mapp) = (totalWeight, anyoneToBlame)
  where
    totalWeight = (+ weight) $ sum $ map fst nexts
    anyoneToBlame = fromMaybe isItOneOfMine $ find isJust $ map snd nexts
    nexts :: [Answer2]
    nexts = map (\cName-> maybe (0, Nothing) (\c->findWrong (c,mapp)) $ M.lookup cName mapp) children
    isItOneOfMine
      | (<=1) (length $ group $ map fst nexts) = Nothing  -- no, they're all the same
      | otherwise = Just $ zip (map fst nexts) $ map (\cName -> fromJust $ M.lookup cName mapp) children

answerTheQuestion :: [(Weight, Prog)] -> Weight
answerTheQuestion input =
  wrongProgWeight + (rightWeight - wrongWeight)
    where
      (wrongWeight, (_,wrongProgWeight,_,_)):_ = wrong
      (rightWeight, _):_ = right
      wrong:right:_ = sortOn length $ groupBy (\a b -> fst a == fst b) $ sort input


main = do
  input <- (absorbErrors . map parseLine . lines) <$> readFile "Day7.txt" :: IO [Program]
  --mapM_ (putStrLn . show) $ input
  let tree = toTree input
  putStrLn . (\(name,_,_,_)->name) . fst $ tree
  putStrLn . show $ answerTheQuestion $ fromJust.snd $ findWrong tree
