module Codewars.G964.FindEven where

findEvenIndex :: [Int] -> Int
findEvenIndex l =
  helper 0 0 (sum l) l
  where
    helper idx suml sumr x
      | null x = -1
      | suml == sumr - head x = idx
      | otherwise = helper (idx + 1) (suml + head x) (sumr - head x) (tail x)
