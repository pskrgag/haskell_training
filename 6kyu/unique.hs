module CodeWars.UniqueNumber where

import Data.Bits

getUnique :: [Int] -> Int
getUnique xs =
  let x :: (Int -> Int -> Bool, Int) = if foldr xor 0 (take 2 xs) == 0 then ((/=), head xs) else ((==), foldr xor 0 $ take 3 xs)
   in foldr (\a b -> b + if fst x (snd x) a then a else 0) 0 xs
