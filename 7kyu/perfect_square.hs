module PerfectSquare where

findNextSquare :: Integer -> Integer
findNextSquare x =
  let fp = fractionalPart $ sqrt $ fromIntegral x
   in if fp == 0.0 then ((^ 2) . (+ 1) . floor) (sqrt $ fromIntegral x) else -1
  where
    fractionalPart x = x - fromIntegral (floor x)
