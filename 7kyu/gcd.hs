module Main where

mgcd :: Int -> Int -> Int

mgcd a b
  | b == 0 = a
  | otherwise = mgcd b (a `mod` b)

main :: IO ()
main = print $ mgcd 10 5
