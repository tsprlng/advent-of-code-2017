module Day20 where

import Text.ParserCombinators.Parsec
import Data.List (sort)

type Vec3 = (Integer,Integer,Integer)
type Particle = (Vec3, Vec3, Vec3) -- pos vel acc

addV (x, y, z) (x', y', z') = (x+x', y+y', z+z')

particleParser :: GenParser Char st Particle
particleParser = vector "p" >>= \p -> vector "v" >>= \v -> vector "a" >>= \a -> return (p, v, a)
  where
    vector label = optional (string ",") >> many space >> between (string (label++"=<")) (string ">") components >>= \comps -> return comps
    components = number >>= \a -> string "," >> number >>= \b -> string "," >> number >>= \c -> return (a, b, c)
    number = many space >> option "" (string "-") >>= \sign -> many digit >>= \digits -> return (read (sign++digits))

parseParticle = either (error.show) id <$> parse particleParser ""

update particle@(p, v, a) = (addV p v', v', a) where v' = addV v a

destroyColliding ((p@(pos,_,_)):(p'@(pos',_,_)):ps)
  | pos == pos' = destroyColliding $ dropWhile (\(p,v,a)-> p==pos) ps
  | otherwise = p : destroyColliding (p':ps)
destroyColliding ps = ps

updateAll = destroyColliding . sort . map update

manh (a,b,c) = abs a + abs b + abs c

main = do
  input <- map parseParticle . lines <$> readFile "Day20.txt"
  putStrLn . show . snd $ minimum $ map (\(ix, (p,v,a))-> ((manh a, manh v, manh p), ix)) $ zip [0..] input
  putStrLn . show $ length $ (iterate updateAll input) !! 1000  -- TODO this is lame. Detect end of collisions?
