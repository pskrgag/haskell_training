module Population where

import Debug.Trace (trace)

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p =
  year p0 percent aug p 0
  where
    year :: Int -> Double -> Int -> Int -> Int -> Int
    year p0 percent aug p c
      | p0 >= p = c
      | otherwise = year (p0 + aug + (floor . (/ 100) .(* percent) . fromIntegral) p0) percent aug p c + 1
