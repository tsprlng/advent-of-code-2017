module Day15 where

generator :: Integer -> Integer -> [Integer]
generator factor = tail . iterate (\x -> mod (x*factor) 2147483647)

genA1 = generator 16807
genB1 = generator 48271

genA2 = filter ((==0).(flip mod 4)) . genA1
genB2 = filter ((==0).(flip mod 8)) . genB1

trunc = flip mod (256*256)

pairs1 a b len = length $ filter id $ take len $ zipWith (==) (map trunc $ genA1 a) (map trunc $ genB1 b)
pairs2 a b len = length $ filter id $ take len $ zipWith (==) (map trunc $ genA2 a) (map trunc $ genB2 b)

main = do
  putStrLn . show $ pairs1 516 190 40000000
  putStrLn . show $ pairs2 516 190  5000000
