module Codewars.Kata.Compare where

import Data.List

comp :: [Integer] -> [Integer] -> Bool
comp as bs = sort (map (^2) as) == sort bs
