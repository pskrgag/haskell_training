module CamelCase (toCamelCase) where

import Data.Char

toCamelCase :: String -> String
toCamelCase s =
  if null s then [] else helper [] False s
  where
    helper acc need (x : xs)
      | null xs = acc ++ (if need then [toUpper x] else [x])
      | need = helper (acc ++ [toUpper x]) False xs
      | otherwise = case x of
          '-' -> helper acc True xs
          '_' -> helper acc True xs
          _ -> helper (acc ++ [x]) False xs
