module PIN where

getCombs :: Char -> [Char]
getCombs '1' = "124"
getCombs '2' = "1235"
getCombs '3' = "236"
getCombs '4' = "1457"
getCombs '5' = "24568"
getCombs '6' = "3659"
getCombs '7' = "478"
getCombs '8' = "57890"
getCombs '9' = "689"
getCombs '0' = "08"

getComb :: Char -> [String]
getComb c = foldl (\x y -> [y] : x) [] (getCombs c)

getPin :: String -> [String] -> [String]
getPin (x : xs) [] = getPin xs (getComb x)
getPin [] old = old
getPin (x : xs) old = getPin xs [b ++ a | a <- getComb x, b <- old]

getPINs :: String -> [String]
getPINs s = getPin s []
