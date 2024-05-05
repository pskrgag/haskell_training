module Codewars.Exercise.MergeChecker where

import Debug.Trace (trace)

isMerge :: String -> String -> String -> Bool
isMerge [] [] [] = True
isMerge s [] [] = False
isMerge [] s [] = False
isMerge [] [] s = False
isMerge [] pp1 pp2 = False
isMerge s pp1 [] = s == pp1
isMerge s [] pp2 = s == pp2
isMerge s@(x : xs) pp1@(p1 : p1s) pp2@(p2 : p2s)
  | (x /= p1) && x /= p2 = False
  | x == p1 = if not (isMerge xs p1s pp2) then isMerge xs pp1 p2s else True
  | x == p2 = isMerge xs pp1 p2s
  | otherwise = False
