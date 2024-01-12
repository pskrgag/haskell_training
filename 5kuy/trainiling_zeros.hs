module Zeros where

zeros :: Int -> Int
zeros x =
  helper x 0 5
  where
   helper x n d
     | x `div` d == 0 = n
     | otherwise = helper x (n + x `div` d) (d * 5)
