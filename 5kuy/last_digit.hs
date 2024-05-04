module LastDigit where

lastDigit :: Integer -> Integer -> Integer
lastDigit base power =
  let circle = helper base (power - 1) (base `mod` 10) []
   in undefined ""
  where
    helper base 0 last [] = [last]
    helper base power last list = list ++ helper base (power - 1) ((last * base) `mod` 10)
