module Mod35 where

solution :: Integer -> Integer
solution number = 
  let l = [1..number - 1]
  in sum $ filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) l
