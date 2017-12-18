module Day18 where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Data.Array (Array, bounds, (!), array)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Map as M


data Value =
  Register Char | Const Int
  deriving (Eq, Show)

data Instruction =
  Snd Value | Rcv Value | Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Jgz Value Value
  deriving (Eq, Show)

instrParser :: GenParser Char st Instruction
instrParser = choice $ map try [snd, set, add, mul, mod, rcv, jgz]
  where
    snd :: GenParser Char st Instruction
    snd = string "snd " >>  value >>= \v ->  return (Snd v)
    rcv = string "rcv " >>  value >>= \v ->  return (Rcv v)

    set = string "set " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Set r v)
    add = string "add " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Add r v)
    mul = string "mul " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Mul r v)
    mod = string "mod " >>  regName >>= \r ->  string " " >>  value >>= \v ->  return (Mod r v)

    jgz = string "jgz " >>  value  >>= \v1 ->  string " " >>  value >>= \v2 -> return (Jgz v1 v2)

    value = number <|> (count 1 anyChar >>= \c -> return (Register (head c)))

    regName = count 1 anyChar >>= \c -> return (head c)
    number = do
      sign <- option "" $ count 1 (oneOf "-+")
      num <- many1 digit
      return (Const (read (sign++num)))

parseInstr :: String -> Instruction
parseInstr = either (error.show) id <$> parse instrParser ""


type Registers = M.Map Char Int

doInstr1 :: (Int, Registers, Array Int Instruction, Maybe Int) -> Maybe (Int, Registers, Array Int Instruction, Maybe Int)
doInstr1 (pc, regs, instrs, _lastFreq)
  | lb <= pc && pc < ub = Just (next instr, regsAfter instr, instrs, maybeRcvFreq instr)
  | otherwise = Nothing
  where
    (lb, ub) = bounds instrs  -- TODO surely a better way
    instr = instrs ! pc

    next (Jgz v1 v2) = pc + (if resolve v1 > 0 then resolve v2 else 1)
    next _ = pc + 1

    maybeRcvFreq (Rcv v) = if resolve v /= 0 then Just (resolve (Register '`')) else Nothing
    maybeRcvFreq _ = Nothing

    regsAfter (Snd v) = M.insert '`' (resolve v) regs  -- '`' is special sound register I use to make life easier
    regsAfter (Jgz _ _) = regs
    regsAfter (Rcv _) = regs
    regsAfter (Set rName v) = M.insert rName (resolve v) regs
    regsAfter (Add rName v) = M.insert rName (resolve (Register rName) + resolve v) regs
    regsAfter (Mul rName v) = M.insert rName (resolve (Register rName) * resolve v) regs
    regsAfter (Mod rName v) = M.insert rName (resolve (Register rName) `mod` resolve v) regs

    resolve (Const int) = int
    resolve (Register r) = maybe 0 id $ M.lookup r regs

doInstr2 :: (Int, Registers, Array Int Instruction, Maybe Int, [Int]) -> Maybe (Int, Registers, Array Int Instruction, Maybe Int, [Int])
doInstr2 (pc, regs, instrs, _lastSend, queue)
  | lb <= pc && pc < ub = Just (next instr, regsAfter instr, instrs, maybeSend instr, maybeConsumed queue instr)
  | otherwise = Nothing
  where
    (lb, ub) = bounds instrs  -- TODO surely a better way
    instr = instrs ! pc

    next (Jgz v1 v2) = pc + (if resolve v1 > 0 then resolve v2 else 1)
    next _ = pc + 1

    maybeSend (Snd v) = Just $ resolve v
    maybeSend _ = Nothing

    maybeConsumed queue (Rcv _) = tail queue
    maybeConsumed queue _ = queue

    regsAfter (Rcv (Register rName)) = M.insert rName (head queue) regs
    regsAfter (Snd _) = regs
    regsAfter (Jgz _ _) = regs
    regsAfter (Set rName v) = M.insert rName (resolve v) regs
    regsAfter (Add rName v) = M.insert rName (resolve (Register rName) + resolve v) regs
    regsAfter (Mul rName v) = M.insert rName (resolve (Register rName) * resolve v) regs
    regsAfter (Mod rName v) = M.insert rName (resolve (Register rName) `mod` resolve v) regs

    resolve (Const int) = int
    resolve (Register r) = maybe 0 id $ M.lookup r regs

steps1 :: Array Int Instruction -> [(Int, Registers, Array Int Instruction, Maybe Int)]
steps1 instrs = map fromJust $ takeWhile isJust $ iterate (doInstr1.fromJust) $ Just (0, M.empty, instrs, Nothing)

steps2 :: Array Int Instruction -> [Int] -> Int -> [(Int, Registers, Array Int Instruction, Maybe Int, [Int])]
steps2 instrs rcvQueue pid = map fromJust $ takeWhile isJust $ iterate (doInstr2.fromJust) $ Just (0, initReg, instrs, Nothing, rcvQueue)
  where
    initReg = M.insert 'p' pid M.empty

duetMessages instrs = (queue0, queue1)  -- TODO I bet this doesn't handle deadlock properly!
  where
    s0 = steps2 instrs queue1 0
    s1 = steps2 instrs queue0 1
    queue0 = map fromJust $ filter isJust $ map (\(_,_,_,maybeSent,_) -> maybeSent) s0
    queue1 = map fromJust $ filter isJust $ map (\(_,_,_,maybeSent,_) -> maybeSent) s1

main = do
  instrsList <- map parseInstr . lines <$> readFile "Day18.txt"
  let instrs = array (0, length instrsList - 1) $ zip [0..] instrsList

  putStrLn . show $ fromJust . head . dropWhile isNothing $ (\(_,_,_,maybeFreq)-> maybeFreq) <$> steps1 instrs
  putStrLn . show $ length $ snd $ duetMessages instrs
