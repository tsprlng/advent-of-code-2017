module Day25 where

import Prelude hiding (Left, Right)
import Data.List (find)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (State)

data Direction = Left | Right
type Tape = ([Bool], Bool, [Bool])

write :: Tape -> Bool -> Tape
write (l, _, r) v = (l, v, r)

shift :: Direction -> Tape -> Tape
shift Left  ([], v, r) = ([], False, v:r)  -- default to False
shift Right (l, v, []) = (v:l, False, [])
shift Left  ((v':l), v, r) = (l, v', v:r)
shift Right (l, v, (v':r)) = (v:l, v', r)

initTape = ([], False, []) :: Tape


type StateName = Char

data State = State {
  stateName :: StateName,
  ifFalse :: StateInstructions,
  ifTrue :: StateInstructions }

data StateInstructions = StateInstructions {
  writeValue :: Bool,
  shiftDirection :: Direction,
  nextState :: StateName }


step :: [State] -> (Tape, State) -> (Tape, State)
step states (tape@(_,v,_), state) = (tape', state')
  where
    instr = if v then ifTrue state else ifFalse state
    tape' = shift (shiftDirection instr) $ write tape (writeValue instr)
    state' = fromJust $ find ((==(nextState instr)).stateName) states

checksum :: Tape -> Int
checksum (l, v, r) = (if v then (+1) else id) $ length $ filter id (l++r)


inputParser :: GenParser Char st ([State], StateName, Int)
inputParser = do
  initState <- string "Begin in state " >> count 1 anyChar >>= \stateName -> string ".\n" >> return stateName
  numSteps <- string "Perform a diagnostic checksum after " >> many digit >>= \numSteps -> string " steps.\n" >> return numSteps
  many $ string "\n"

  states <- many $ do
    stateName <- string "In state " >> count 1 anyChar >>= \stateName -> string ":\n" >> return stateName
    many (string " ") >> string "If the current value is 0:\n"
    ifFalse <- stateInstructions
    many (string " ") >> string "If the current value is 1:\n"
    ifTrue <- stateInstructions
    many $ string "\n"
    return $ State (head stateName) ifFalse ifTrue

  return (states, head initState, read numSteps)

    where
      stateInstructions = do
        many (string " ")
        writeValue <- string "- Write the value " >> (trueVal <|> falseVal) >>= \v -> string ".\n" >> return v
        many (string " ")
        shiftDirection <- string "- Move one slot to the " >> (leftVal <|> rightVal) >>= \d -> string ".\n" >> return d
        many (string " ")
        nextState <- string "- Continue with state " >> count 1 anyChar >>= \s -> string ".\n" >> return s
        return $ StateInstructions writeValue shiftDirection (head nextState)

      trueVal = string "1" >> return True
      falseVal = string "0" >> return False
      leftVal = string "left" >> return Left
      rightVal = string "right" >> return Right


parseInput input = either (error.show) id $ parse inputParser "" input

test = if result == 3 then () else (error $ "Result was supposed to be 3 but was " ++ show result)
  where
    result = checksum.fst $ (!!6) $ iterate (step states) (initTape, firstState)
    states = [
      State 'A' (StateInstructions True Right 'B') (StateInstructions False Left 'B'),
      State 'B' (StateInstructions True Left 'A') (StateInstructions True Right 'A') ]
    firstState = fromJust $ find ((=='A').stateName) states

main = do
  (states, firstStateName, numSteps) <- parseInput <$> readFile "Day25.txt"
  let firstState = fromJust $ find ((==firstStateName).stateName) states
  putStrLn . show $ checksum.fst $ (!!numSteps) $ iterate (step states) (initTape, firstState)
