module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 =
  map (\x -> if isAlpha x && isAscii x then doRot x else x) 
  where
    doRot x =
      let base = if isUpper x then ord 'A' else ord 'a'
       in chr $ ((ord x) - base + 13) `mod` 26 + base
