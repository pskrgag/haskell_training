module DigitalRoot where

digitalRoot :: (Integral a) => a -> a
digitalRoot n =
  if n < 10
    then n
    else digitalRoot $ helper n 0
  where
    helper n acc = if n < 10 then n + acc else helper (n `div` 10) (acc + n `rem` 10)
