module Bus where

number :: [(Int, Int)] -> Int
number =
  foldr (\x y -> y + fst x - snd x) 0
