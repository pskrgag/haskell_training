module Codewars.Kata.FindOdd where

import Data.Bits

findOdd :: [Int] -> Int
findOdd = foldr xor 0
