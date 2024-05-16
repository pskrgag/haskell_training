module Codewars.Kata.Permutations (permutations) where

import Data.List (nub)

splits :: String -> [(String, String)]
splits [] = [([], [])]
splits (a:as) = ([],a:as) : [(a:bs,cs) | (bs,cs) <- splits as]

perms :: String -> [String]
perms [] = [""]
perms (x:xs) = [bs ++ x:cs | p <- perms xs, (bs, cs) <- splits p]

permutations :: String -> [String]
permutations = nub . perms
