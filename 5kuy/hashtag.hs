module Hashtag where

import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag x
  | x == "" = Nothing
  | otherwise =
      let hash = generateHash x
       in if length hash < 140 && length hash > 0 then Just $ "#" <> hash else Nothing
  where
    upFirstLetter (x : xs) = toUpper x : xs
    generateHash x = concat $ map upFirstLetter $ words x
