module Day23 where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Data.Array (Array, bounds, (!), array)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Map as M


data Value =
  Register Char | Const Int
  deriving (Eq, Show)

data Instruction =
  Set Char Value | Sub Char Value | Mul Char Value | Jnz Value Value
  deriving (Eq, Show)

instrParser :: GenParser Char st Instruction
instrParser = choice $ map try [set, sub, mul, jgz]
  where
    set = string "set " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Set r v)
    sub = string "sub " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Sub r v)
    mul = string "mul " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Mul r v)

    jgz = string "jnz " >>  value  >>= \v1 ->  string " " >>  value >>= \v2 -> return (Jnz v1 v2)

    value = number <|> (count 1 anyChar >>= \c -> return (Register (head c)))

    regName = count 1 anyChar >>= \c -> return (head c)
    number = do
      sign <- option "" $ count 1 (oneOf "-+")
      num <- many1 digit
      return (Const (read (sign++num)))

parseInstr :: String -> Instruction
parseInstr = either (error.show) id <$> parse instrParser ""


type Registers = M.Map Char Int

doInstr :: (Int, Registers, Array Int Instruction, Bool) -> Maybe (Int, Registers, Array Int Instruction, Bool)
doInstr (pc, regs, instrs, _wasMul)
  | lb <= pc && pc < ub = Just (next instr, regsAfter instr, instrs, isMul instr)
  | otherwise = Nothing
  where
    (lb, ub) = bounds instrs  -- TODO surely a better way
    instr = instrs ! pc

    isMul (Mul _ _) = True
    isMul _ = False

    next (Jnz v1 v2) = pc + (if resolve v1 /= 0 then resolve v2 else 1)
    next _ = pc + 1

    regsAfter (Jnz _ _) = regs
    regsAfter (Set rName v) = M.insert rName (resolve v) regs
    regsAfter (Sub rName v) = M.insert rName (resolve (Register rName) - resolve v) regs
    regsAfter (Mul rName v) = M.insert rName (resolve (Register rName) * resolve v) regs

    resolve (Const int) = int
    resolve (Register r) = maybe 0 id $ M.lookup r regs

steps, steps2 :: Array Int Instruction -> [(Int, Registers, Array Int Instruction, Bool)]
steps instrs = map fromJust $ takeWhile isJust $ iterate (doInstr.fromJust) $ Just (0, M.empty, instrs, False)
steps2 instrs = map fromJust $ takeWhile isJust $ iterate (doInstr.fromJust) $ Just (0, initRegs, instrs, False)
  where
    initRegs = M.insert 'a' 1 M.empty

main = do
  instrsList <- map parseInstr . lines <$> readFile "Day23.txt"
  let instrs = array (0, length instrsList - 1) $ zip [0..] instrsList

  putStrLn . show $ length $ filter (\(_,_,_,isMul)->isMul) $ steps instrs
  -- putStrLn . show $ (\(_,regs,_,_)-> M.lookup 'h' regs) $ last $ steps2 instrs
  -- blah blah blah reverse engineering
  -- cba
  -- seq 106500 17 123500 | while read n; do (grep -q "^${n}\$" primes || echo "$n" || true); done | wc -l
