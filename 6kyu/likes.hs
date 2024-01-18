module Likes where

likes :: [String] -> String
likes [] = "no one likes this"
likes [x] = x ++ " likes this"
likes [x, y] = x ++ " and " ++ y ++ " like this"
likes [x, y, z] = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
likes (x : y : z : xs) = x ++ ", " ++ y ++ " and " ++ show (length xs + 1) ++ " others like this"
