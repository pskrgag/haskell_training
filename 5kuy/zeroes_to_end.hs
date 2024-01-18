module MovingZeros (moveZeros) where

moveZeros :: [Int] -> [Int]
moveZeros x =
  filter (/= 0) x ++ filter (== 0) x
