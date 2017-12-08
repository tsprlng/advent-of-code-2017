module Day8 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Exception (throw)

-- toChange, delta, toCompare, comparator
type Instruction = (Name, Integer, Name, Integer->Bool)
type Name = String
type Operator = String

type Regs = M.Map Name Integer

xor a = if a then not else id

toFn "<" = flip (<)
toFn ">" = flip (>)
toFn "<=" = flip (<=)
toFn ">=" = flip (>=)
toFn "==" = flip (==)
toFn "!=" = flip (/=)
toFn x = error $ "what is " ++ x

lineParser :: GenParser Char st Instruction
lineParser =
  do toChange <- many (noneOf " ")
     direction <- string " " >> (string "inc" <|> string "dec")
     sign <- string " " >> (option '+' $ oneOf "+-")
     amount <- many digit  -- TODO at least one
     string " if "
     toCompare <- many (noneOf " ")
     operator <- string " " >> many (oneOf "<>=!")  -- TODO is there some sort of map (try string) over <|> ?
     compSign <- string " " >> (option '+' $ oneOf "+-")
     compAmount <- many digit
     let signedAmt = if xor (sign=='-') (direction=="dec") then ((-1)*) else id
     let signedComp = if compSign=='-' then ((-1)*) else id
     return (toChange, signedAmt $ read amount, toCompare, toFn operator $ signedComp $ read compAmount)
  where

parseLine :: String -> Either ParseError Instruction
parseLine = parse lineParser ""

absorbErrors :: [Either ParseError Instruction] -> [Instruction]
absorbErrors = map (either (error.show) id)

doInstruction :: Regs -> Instruction -> Regs
doInstruction regs (toChange, delta, compareWith, comparator)
  | comparator compared = M.alter (Just.(+delta).fromMaybe 0) toChange regs
  | otherwise = regs
  where
    compared = M.findWithDefault 0 compareWith regs

doInstructions :: [Instruction] -> Regs
doInstructions = foldl doInstruction M.empty

highestEver :: [Instruction] -> Integer
highestEver instrs = maximum $ allValueSets
  where
    allValueSets :: [Integer]
    allValueSets = concatMap (map snd . M.toList) $ scanl doInstruction M.empty instrs


main = do
  input <- (absorbErrors . map parseLine . lines) <$> readFile "Day8.txt" :: IO [Instruction]
  --mapM_ (putStrLn . show) $ input
  putStrLn . show $ maximum $ map snd $ M.toList $ doInstructions input
  putStrLn . show $ highestEver input
