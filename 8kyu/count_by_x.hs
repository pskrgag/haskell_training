module Codewars.Kata.Count where

countBy :: Integer -> Int -> [Integer]
countBy x n = take n [x, x + x ..]
