module InfixToPostfix (toPostfix) where

import Data.Char
import Data.List
import Debug.Trace

data Op = Plus | Minus | Div | Mul | Power | OpenBracket
  deriving (Eq)

opLow :: Op -> Op -> Bool
opLow Power Power = False
opLow _ Power = True
opLow Power _ = False
opLow Mul Plus = False
opLow Mul Minus = False
opLow Div Plus = False
opLow Div Minus = False
opLow _ _ = True

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Div = "/"
  show Mul = "*"
  show Power = "^"
  show OpenBracket = ""

splitWhile :: [Op] -> (Op -> Bool) -> ([Op], [Op])
splitWhile xs fn =
  let s = takeWhile fn xs
   in splitAt (length s) xs

pushOpStack :: [Op] -> Op -> (String, [Op])
pushOpStack [] op = ("", [op])
pushOpStack l@(x : xs) OpenBracket = ("", OpenBracket : l)
pushOpStack l@(x : xs) op =
  let (print, old) = splitWhile l (\x -> x /= OpenBracket && opLow op x)
   in (concatMap show print, op : old)

shuntingYard :: String -> [Op] -> String
shuntingYard ('+' : xs) op =
  let (s, ops) = pushOpStack op Plus
   in s ++ shuntingYard xs ops
shuntingYard ('-' : xs) op =
  let (s, ops) = pushOpStack op Minus
   in s ++ shuntingYard xs ops
shuntingYard ('*' : xs) op =
  let (s, ops) = pushOpStack op Mul
   in s ++ shuntingYard xs ops
shuntingYard ('/' : xs) op =
  let (s, ops) = pushOpStack op Div
   in s ++ shuntingYard xs ops
shuntingYard ('^' : xs) op =
  let (s, ops) = pushOpStack op Power
   in s ++ shuntingYard xs ops
shuntingYard ('(' : xs) op =
  let (s, ops) = pushOpStack op OpenBracket
   in s ++ shuntingYard xs ops
shuntingYard (')' : xs) op =
  let (s, opp:opps) = splitWhile op (OpenBracket /=)
   in concatMap show s ++ shuntingYard xs opps
shuntingYard (' ' : xs) op = shuntingYard xs op
shuntingYard (x : xs) op = x : shuntingYard xs op
shuntingYard [] op = concatMap show op

toPostfix :: String -> String
toPostfix xs = shuntingYard xs []
