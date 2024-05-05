module DirRed where

data Direction = North | East | West | South deriving (Eq, Show)

dirReduce' :: [Direction] -> [Direction]
dirReduce' (North : South : xs) = dirReduce' xs
dirReduce' (South : North : xs) = dirReduce' xs
dirReduce' (East : West : xs) = dirReduce' xs
dirReduce' (West : East : xs) = dirReduce' xs
dirReduce' (x : xs) = x : dirReduce' xs
dirReduce' [] = []


dirReduce :: [Direction] -> [Direction]
dirReduce = helper 0 
  where 
   helper n xs = 
      let new = dirReduce' xs
         in if n /= length new then helper (length new) new else new
