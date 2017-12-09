module Day9 where

import Text.ParserCombinators.Parsec

data Stuff = Group [Stuff] | Garbage String
  deriving (Eq, Show)

stuffParser :: GenParser Char st Stuff
stuffParser = garbage <|> group
  where
    group = (between (string "{") (string "}") $ sepBy stuffParser (string ",")) >>=
      return . Group
          -- TODO surely there's a right-associative way to do this?

    garbage = between (string "<") (string ">") $ many (ignore <|> (count 1 $ noneOf ">")) >>=
      return . Garbage . concat
          -- TODO could filter Maybes or something instead (but [] seems more or less equivalent?)
      where
        ignore = char '!' >> anyChar >> return ""

parseStuff = either (error.show) id <$> parse stuffParser ""

score :: Stuff -> Int
score = score' 1
  where
    score' accum (Group xs) = (accum +) $ sum (map (score' (accum+1)) xs)
    score' _ (Garbage _) = 0

countGarbage :: Stuff -> Int
countGarbage (Group xs) = sum $ map countGarbage xs
countGarbage (Garbage s) = length s


main = do
  input <- parseStuff <$> readFile "Day9.txt" :: IO Stuff
  putStrLn . show $ score input
  putStrLn . show $ countGarbage input
