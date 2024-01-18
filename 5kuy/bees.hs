module MinFree where

import Data.List

minfree :: [Int] -> Int
minfree = helper (-1) . sort
  where
    helper base s =
      case s of
        (x : xs) -> if base + 1 == x then helper x xs else base + 1
        [] -> base + 1
