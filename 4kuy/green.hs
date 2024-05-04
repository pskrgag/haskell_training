module Green where

numDigits :: Int -> Int
numDigits = (+) 1 . floor . logBase 10 . fromIntegral

green :: Int -> Bool
green x = (x * x `mod` (10 ^ numDigits x)) == x

greenFast x
  | x >= 1000 && not (x `mod` 1000 == 625 || x `mod` 1000 == 376) = False
  | otherwise = green x

minPairs :: [Int] -> (Int -> Int -> Int) -> [Int]
minPairs [x,y] f = [f y x]
minPairs (x:y:xs) f = f y x : minPairs (y:xs) f

--      09 376
--      90 625
--  01  09 376
--  08  90 625
--  28  90 625
--  71  09 376
--  128 90 625
--  871 09 376
-- 2128 90 625
