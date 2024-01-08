module Main where

-- Given a random non-negative number, you have to return the digits of this number within an array in reverse order.
-- 35231 => [1,3,2,5,3]
-- 0 => [0]

makeArray :: Int -> [Int]
makeArray num =
  if num == 0
    then [0]
    else helper num []
  where
    helper :: Int -> [Int] -> [Int]
    helper n list
      | n == 0 = list
      | otherwise = helper (n `div` 10) (list ++ [n `mod` 10])

main :: IO ()
main = print $ makeArray 123
