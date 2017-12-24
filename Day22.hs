{-# Language BangPatterns #-}

module Day22 where

import Data.List (intercalate)
import Data.Array.IArray (array, (!), (//), elems, bounds, assocs, listArray)
import Data.Array.Unboxed (UArray)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (writeArray, readArray, newListArray, freeze)

import System.IO.Unsafe (unsafeInterleaveIO)

type Vec2 = (Int, Int)  -- (0,1) is up; (1,0) is right
type Position = Vec2
type Direction = Vec2

addV (a,b) (c,d) = (a+c, b+d)

type Map = UArray Position Bool

-- TODO hrrrrrrm
turnL, turnR :: Direction -> Direction
turnL (0,1) = (-1,0)
turnL (0,-1) = (1,0)
turnL (1,0) = (0,1)
turnL (-1,0) = (0,-1)
turnR (1,0) = (0,-1)
turnR (-1,0) = (0,1)
turnR (0,-1) = (-1,0)
turnR (0,1) = (1,0)

initPosn :: Map -> Position
initPosn map = (div (w+1) 2, div (h+1) 2) where (_,(w,h)) = bounds map
initDirn = (0,1)

readMap :: String -> Map
readMap map = array ((0,0),(length (head linez) - 1, length linez - 1)) $ concatMap snortLine $ zip [0..] $ linez
  where
    linez = reverse $ lines map
    snortLine (n,line) = zipWith (\m c-> ((m,n), c=='#')) [0..] line

showMap :: Map -> String
showMap marp = intercalate "\n" $ map formatLine [h,(h-1)..hh]
  where
    formatLine n = map (\m -> if marp ! (m,n) then '#' else '.') [ww..w]
    ((ww,hh),(w,h)) = bounds marp

showMap2 :: UArray Position VirusState -> String
showMap2 marp = intercalate "\n" $ map formatLine [h,(h-1)..hh]
  where
    formatLine n = map (\m-> toChar (marp ! (m,n))) [ww..w]
    ((ww,hh),(w,h)) = bounds marp
    toChar 0 = '.'
    toChar 1 = 'W'
    toChar 2 = '#'
    toChar 3 = 'F'
    toChar n = head $ show n

steppy :: (Position, Direction, Map, Map) -> (Bool, (Position, Direction, Map, Map))
steppy (posn, dirn, map, origMap) = (outcome, (addV posn dirn', dirn', map', origMap))
  where
    dirn' = if (map ! posn) then turnR dirn else turnL dirn
    map' = map // [(posn, outcome)]
    outcome = not (map ! posn)

type VirusState = Int
statClean = 0
statWeak = 1
statInfected = 2
statFlagged = 3

outcome' n = mod (n+1) 4
durnn 0 dir = turnL dir
durnn 1 dir = dir
durnn 2 dir = turnR dir
durnn 3 dir = (-a,-b) where (a,b) = dir

steppy2 :: Map2 -> (Position, Direction) -> IO (VirusState, (Position, Direction))
steppy2 map !(posn, dirn) = do
  state <- readArray map posn
  let  dirn' = durnn state dirn
  let  outcome = outcome' state
  writeArray map posn outcome
  return (outcome, (addV posn dirn', dirn'))

type Map2 = IOUArray Position VirusState
upgrade :: Bool -> VirusState
upgrade b = if b then 2 else 0

iterateM :: (a -> IO a) -> a -> IO [a]
iterateM f bla = do
  first <- f bla
  rest <- unsafeInterleaveIO $ iterateM f first  -- lazy like normal iterate (TODO is there nicer way?)
  return (first:rest)

-- TODO experiment with wrapping unsafePerformIOness back into pure fn that hides the MArray completely?

main = do
  input <- readMap <$> readFile "Day22.txt"
  let demoInput = readMap "..#\n#..\n..."
  --putStrLn . show $ bounds input
  --putStrLn . show $ initPosn input

  let expanded = listArray ((-300,-300),(300,300)) (repeat False) // (assocs input) :: UArray Position Bool
  -- TODO fixed bounds are lame. Should be using a strict map of states or something.
  --let demoExpnd = listArray ((-10,-10),(10,10)) (repeat False) // (assocs demoInput)
  let demoExpnd = listArray ((-300,-300),(300,300)) (repeat False) // (assocs demoInput) :: UArray Position Bool
  --let demo = take 70 $ tail $ iterate (steppy.snd) (False, (initPosn input, initDirn, expanded, expanded))
  --mapM_ (putStrLn . showMap . (\(_,_,map,_)->map) . snd) $ demo
  --putStrLn . showMap . (\(_,_,map,_)->map) . snd $ last $ demo

  expanded2 <- (newListArray (bounds expanded) (map upgrade $ elems expanded)) :: IO Map2
  demoexpanded2 <- (newListArray (bounds demoExpnd) (map upgrade $ elems demoExpnd)) :: IO Map2

  --whatwegot <- freeze demoexpanded2
  --putStrLn . showMap2 $ whatwegot

  putStrLn . show $ length $ filter (fst) $ take 10000 $ tail $ iterate (steppy.snd) (False, (initPosn input, initDirn, expanded, expanded))
  --putStrLn . show $ length $ filter ((==statInfected).fst) $ take 10000000 $ tail $ iterate (steppy2.snd) (statClean, (initPosn input, initDirn, expanded2, expanded2))

  --let demo = take 5 $ tail $ iterate (steppy2.snd) (0, (initPosn demoInput, initDirn, demoexpanded2, demoexpanded2))
  --mapM_ (putStrLn . showMap2 . (\(_,_,map,_)->map) . snd) $ demo
  steps <- iterateM ((steppy2 expanded2).snd) (0, (initPosn input, initDirn))
  putStrLn . show $ length $ filter ((==statInfected).fst) $ take 10000000 steps

  --putStrLn . show $ length $ filter ((==statInfected).fst) $ take 100000 $ tail $ steps
