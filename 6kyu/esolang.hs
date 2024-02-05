module Esolang where

import Data.Char

myFirstInterpreter :: String -> String
myFirstInterpreter = helper 0
  where
    helper n [] = ""
    helper n ('+' : xs) = helper ((n + 1) `mod` 256) xs
    helper n ('.' : xs) = [chr n] ++ helper n xs
    helper n (x : xs) = helper n xs
