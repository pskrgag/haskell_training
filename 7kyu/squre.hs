module Main where

import Debug.Trace (trace)

squareDigit :: Int -> Int
squareDigit num =
  let res = read (concatMap (show . square) (digs $ abs num))
   in if num == 0 then 0 else if num < 0 then -res else res
  where
    square num = num * num
    digs x = if x == 0 then [] else digs (x `div` 10) ++ [x `mod` 10]

main :: IO ()
main = print $ squareDigit 0
