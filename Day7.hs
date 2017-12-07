module Day7 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (either)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (find, group, sort, groupBy, sortOn)
import Control.Exception (throw)
import Control.Monad (liftM)


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
toTree inputs = (maybe (error "wat") id $ find (\(_,_,parent,_)-> isNothing parent) $ map snd $ M.toList finalMap, finalMap)
  where
    finalMap = foldl upd M.empty inputs
      where
        upd :: Collector -> (Name, Weight, [Name]) -> Collector
        upd map (name, weight, children) = foldl upChild (upSelf map) children
          where
            upChild :: Collector -> Name -> Collector
            upChild mp nm = M.alter (Just.(\(_,w,p,c)->(nm,w,Just name,c)).(maybe (def nm) id)) nm mp
            upSelf :: Collector -> Collector
            upSelf = M.alter (Just.(\(_,_,p,_)->(name, weight, p, children)).(maybe (def name) id)) name
            def name = (name, -1, Nothing, [])

type Answer2 = (Weight, Maybe [(Weight,Prog)])
findWrong :: (Prog, Collector)->Answer2
findWrong (root@(_,weight,_,children), mapp) =
  ((weight+).sum $ map fst nexts, maybe isItMe id $ find isJust $ map snd nexts)
    where
      nexts :: [Answer2]
      nexts = map (\cName-> maybe (0, Nothing) (\c->findWrong (c,mapp)) $ M.lookup cName mapp) children
      isItMe
        | 2 > (length $ group $ map fst nexts) = Nothing
        | otherwise = Just $ zip (map fst nexts) $ map (\cName -> maybe (error "waat") id $ M.lookup cName mapp) children

answerTheQuestion :: [(Weight, Prog)] -> Weight
answerTheQuestion input =
  wrongProgWeight + (rightWeight - wrongWeight)
    where
      (wrongWeight, (_,wrongProgWeight,_,_)):_ = wrong
      (rightWeight, _):_ = right
      wrong:right:_ = sortOn length $ groupBy (\a b -> fst a == fst b) $ sort input

main = do
  input <- liftM (absorbErrors . map parseLine . lines) $ readFile "Day7.txt" :: IO [Program]
  --mapM_ (putStrLn . show) $ input
  let tree = toTree input
  putStrLn . (\(name,_,_,_)->name) . fst $ tree
  putStrLn . show $ answerTheQuestion $ fromJust.snd $ findWrong tree
