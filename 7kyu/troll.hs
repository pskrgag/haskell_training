module Main where

import Data.Char

disemvowel :: String -> String
disemvowel =
  filter helper
  where
    helper :: Char -> Bool
    helper a =
      let c = toLower a
       in not (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')

main :: IO ()
main = print $ disemvowel "hello"
