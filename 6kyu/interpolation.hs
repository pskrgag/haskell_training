module Gradients where

data RGB = RGB Int Int Int

gradientFn :: RGB -> RGB -> (Int -> RGB)
gradientFn (RGB r1 g1 b1) (RGB r2 g2 b2) =
  \scale -> RGB (r1 + diffr * scale `div` 100) (g1 + diffg * scale `div` 100) (b1 + diffb * scale `div` 100)
  where
    diffr = r2 - r1
    diffg = g2 - g1
    diffb = b2 - b1
