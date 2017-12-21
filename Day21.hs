module Day21 where

import Text.ParserCombinators.Parsec
import Data.Array.IArray (array, (!), (//), elems, bounds, assocs, ixmap)
import Data.Array.Unboxed (UArray)
import Data.List (intercalate)

type Pattern = UArray (Int,Int) Bool
type Rule = (Pattern, Pattern)

addV (a,b) (c,d) = (a+c, b+d)

toPattern :: [[Bool]] -> Pattern
toPattern rows = array ((0,0), (length (head rows) - 1, length rows - 1)) $ concatMap mapRow $ zip [0..] rows
  where
    mapRow :: (Int, [Bool]) -> [((Int, Int), Bool)]
    mapRow (rowId, row) = map (\(colId, cell)-> ((colId, rowId), cell)) $ zip [0..] row

ruleParser :: GenParser Char st Rule
ruleParser = do
  lPattern <- patternParser
  string " => "
  rPattern <- patternParser
  return (lPattern, rPattern)

patternParser = toPattern <$> sepBy row (string "/")
  where
    row = many (oneOf ".#") >>= \r -> return (map (=='#') r)

parseRule = either (error.show) id <$> parse ruleParser ""
parsePattern = either (error.show) id <$> parse patternParser ""

formatPattern :: Pattern -> String
formatPattern pattern = intercalate "\n" $ map formatRow [0..rs]
  where
    formatRow :: Int -> String
    formatRow r = map (\c -> if (pattern ! (c,r)) then '#' else '.') [0..cs]
    ((_,_),(cs,rs)) = bounds pattern

initPattern = parsePattern ".#./..#/###"

splitRules :: [Rule] -> ([Rule], [Rule])
splitRules rules = (twos, threes)
  where
    twos = filter ((==1) . snd . snd . bounds . fst) rules
    threes = filter ((==2) . snd . snd . bounds . fst) rules

completeRules :: [Rule] -> [Rule]
completeRules rules = hflip $ vflip $ r90 rules
  where
    hflip rules = rules ++ map (proc hflip') rules
    vflip rules = rules ++ map (proc vflip') rules
    r90 rules = rules ++ map (proc r90') rules
    proc f (from, to) = (ixmap (bounds from) (f size) from, to)
      where
        (_,(size,_)) = bounds from
    hflip' size (x,y) = (size-x,y)
    vflip' size (x,y) = (x,size-y)
    r90' size (x,y) = (y,x)

expandImage :: [Rule] -> Pattern -> Pattern
expandImage rules image
  | mod (lastCol+1) 2 == 0 = expand (zip [0,2..lastCol] [0,3..((lastCol+1)*3 `div` 2)]) ((lastCol+1)*3 `div` 2) twoRules image
  | mod (lastCol+1) 3 == 0 = expand (zip [0,3..lastCol] [0,4..((lastCol+1)*4 `div` 3)]) ((lastCol+1)*4 `div` 3) threeRules image
  where
    (_,(lastCol,_)) = bounds image
    (twoRules, threeRules) = splitRules $ completeRules rules

    expand :: [(Int,Int)] -> Int -> [Rule] -> Pattern -> Pattern
    expand posMaps newSize rules image
      = foldl expandChunk (array ((0,0),(newSize-1, newSize-1)) []) [((oldX, oldY), (newX, newY)) | (oldX, newX)<-posMaps, (oldY, newY)<-posMaps]
        where
          expandChunk :: Pattern -> ((Int,Int),(Int,Int)) -> Pattern
          expandChunk intoPatt ((oldX, oldY), (newX, newY)) = intoPatt // map (\(pos, val)->(addV pos (newX,newY), val)) (assocs $ findExpn)
            where
              findExpn :: Pattern
              findExpn = snd . head $ filter (isRelevant . fst) $ rules
              isRelevant :: Pattern -> Bool
              isRelevant matchPattern = all (\((x,y), val)-> val == (image ! (oldX+x, oldY+y))) $ assocs matchPattern

main = do
  rules <- map parseRule . lines <$> readFile "Day21.txt"
  putStrLn . show $ length $ filter (=='#') $ formatPattern $ (!! 5) $ iterate (expandImage rules) initPattern
  putStrLn . show $ length $ filter (=='#') $ formatPattern $ (!! 18) $ iterate (expandImage rules) initPattern
