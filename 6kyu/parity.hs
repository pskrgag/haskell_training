module FindOutlier where

findOutlier :: [Int] -> Int
findOutlier xs =
  -- Sum  3 -- need to find even
  -- Sum  2 -- need to find even
  -- Sum  1 -- need to find odd
  -- Sum  0 -- need to find odd
  let s = sum (map (`mod` 2) (take 3 xs))
   in head (filter (\x -> x `mod` 2 == boolToInt (s < 2)) xs)
  where
    boolToInt True = 1
    boolToInt False = 0
