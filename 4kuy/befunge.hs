module Befunge93 where

import Control.Exception
import Data.Char
import Debug.Trace (trace)
import System.Random (StdGen, newStdGen, uniformR)
import Text.Printf
import Prelude hiding (Left, Right)

data MoveCursor = Right | Left | Up | Down
  deriving (Show)

data IntState = IntState {string :: Bool, stack :: [Int], pos :: (Int, Int), move :: MoveCursor}
  deriving (Show)

getInst :: IntState -> [String] -> Char
getInst IntState {string, stack, pos, move} code = (code !! snd pos) !! fst pos

nextPos :: [String] -> (Int, Int) -> MoveCursor -> (Int, Int)
nextPos code (x, y) Right = if x == length (code !! y) - 1 then (0, y) else (x + 1, y)
nextPos code (x, y) Left = if x == 0 then (length (code !! y) - 1, y) else (x - 1, y)
nextPos code (x, y) Up = if y == 0 then (x, length code - 1) else (x, y - 1)
nextPos code (x, y) Down = if y == length code - 1 then (x, 0) else (x, y + 1)

opStack :: (Int -> Int -> Int) -> [Int] -> [Int]
opStack fn (a : b : xs) = fn b a : xs

opUnStack :: (Int -> Int) -> [Int] -> [Int]
opUnStack fn (a : xs) = fn a : xs

popStack :: [Int] -> (Int, [Int])
popStack (a : xs) = (a, xs)

pop3Stack :: [Int] -> (Int, Int, Int, [Int])
pop3Stack (a : b : c : xs) = (a, b, c, xs)

pop2Stack :: [Int] -> (Int, Int, [Int])
pop2Stack (a : b : xs) = (a, b, xs)

dupStack :: [Int] -> [Int]
dupStack [] = [0]
dupStack (a : xs) = a : a : xs

swapStack :: [Int] -> [Int]
swapStack [a] = [0, a]
swapStack (a : b : xs) = b : a : xs

random :: StdGen -> (StdGen, MoveCursor)
random g =
  let v = uniformR (1 :: Int, 4 :: Int) g
   in case fst v of
        1 -> (snd v, Right)
        2 -> (snd v, Left)
        3 -> (snd v, Up)
        4 -> (snd v, Down)

changeString :: [String] -> (Int, Int) -> Char -> [String]
changeString code (x, y) ch =
  let (ls, rs) = splitAt y code
      (ls', rs') = (ls, tail rs)
      (l, r) = splitAt x (head rs)
   in ls' ++ [l ++ [ch] ++ tail r] ++ rs'

moveNext :: [String] -> IntState -> IntState
moveNext code state@IntState {string, stack, pos, move} = IntState string stack (nextPos code pos move) move

setDirection :: MoveCursor -> IntState -> IntState
setDirection mv state@IntState {string, stack, pos, move} = IntState string stack pos mv

modifyStack :: ([Int] -> [Int]) -> IntState -> IntState
modifyStack fn state@IntState {string, stack, pos, move} = IntState string (fn stack) pos move

modifyStackAndMove :: [String] -> ([Int] -> [Int]) -> IntState -> IntState
modifyStackAndMove code fn state@IntState {string, stack, pos, move} = moveNext code $ IntState string (fn stack) pos move

toggleString :: IntState -> IntState
toggleString IntState {string, stack, pos, move} = IntState (not string) stack pos move

pushStack :: Int -> IntState -> IntState
pushStack val IntState {string, stack, pos, move} = IntState string (val : stack) pos move

popStackState :: IntState -> (Int, IntState)
popStackState state@IntState {string, stack, pos, move} =
  let (val, st) = popStack stack
   in (val, modifyStack (const st) state)

pop3StackState :: IntState -> (Int, Int, Int, IntState)
pop3StackState state@IntState {string, stack, pos, move} =
  let (v1, v2, v3, st) = pop3Stack stack
   in (v1, v2, v3, modifyStack (const st) state)

--  (trace (show st ++ " " ++ (show $ getInst st cd) ++ "     " ++ show cd))
doInterpret :: StdGen -> [String] -> IntState -> String
doInterpret gen cd st = helper gen cd (getInst st cd) st
  where
    helper :: StdGen -> [String] -> Char -> IntState -> String
    helper gen code inst state
      -- String
      | inst == '"' = doInterpret gen code ((moveNext code . toggleString) state)
      | string state = doInterpret gen code ((moveNext code . pushStack (ord inst)) state)
      -- Stack
      | isNumber inst = doInterpret gen code ((moveNext code . pushStack (digitToInt inst)) state)
      | inst == '+' = doInterpret gen code (modifyStackAndMove code (opStack (+)) state)
      | inst == '-' = doInterpret gen code (modifyStackAndMove code (opStack (-)) state)
      | inst == '*' = doInterpret gen code (modifyStackAndMove code (opStack (*)) state)
      | inst == '/' = doInterpret gen code (modifyStackAndMove code (opStack div) state)
      | inst == '%' = doInterpret gen code (modifyStackAndMove code (opStack mod) state)
      | inst == '!' = doInterpret gen code (modifyStackAndMove code (opUnStack (\x -> if x == 0 then 1 else 0)) state)
      | inst == '`' = doInterpret gen code (modifyStackAndMove code (opStack (\x y -> if x > y then 1 else 0)) state)
      | inst == ':' = doInterpret gen code (modifyStackAndMove code dupStack state)
      | inst == '\\' = doInterpret gen code (modifyStackAndMove code swapStack state)
      | inst == '$' = doInterpret gen code (modifyStackAndMove code (snd . popStack) state)
      -- Moves
      | inst == '>' = doInterpret gen code ((moveNext code . setDirection Right) state)
      | inst == '<' = doInterpret gen code ((moveNext code . setDirection Left) state)
      | inst == '^' = doInterpret gen code ((moveNext code . setDirection Up) state)
      | inst == 'v' = doInterpret gen code ((moveNext code . setDirection Down) state)
      | inst == '?' =
          let (g, r) = random gen
           in doInterpret g code ((moveNext code . setDirection r) state)
      -- If
      | inst == '_' =
          let (e, st) = popStackState state
              mv = if e == 0 then Right else Left
           in doInterpret gen code ((moveNext code . setDirection mv) st)
      | inst == '|' =
          let (e, st) = popStackState state
              mv = if e == 0 then Down else Up
           in doInterpret gen code ((moveNext code . setDirection mv) st)
      -- Print
      | inst == '.' =
          let (e, st) = popStackState state
           in show e ++ doInterpret gen code (moveNext code st)
      | inst == ',' =
          let (e, st) = popStackState state
           in printf "%c" e ++ doInterpret gen code (moveNext code st)
      -- Reflection
      | inst == 'p' =
          let (y, x, v, st) = pop3StackState state
              newString = changeString code (x, y) (chr v)
           in doInterpret gen (assert ((newString !! y) !! x == chr v) newString) (moveNext code st)
      | inst == 'g' = doInterpret gen code (modifyStackAndMove code (opStack (\x y -> ord ((code !! y) !! x))) state)
      -- Special
      | inst == '#' = doInterpret gen code ((moveNext code . moveNext code) state)
      | inst == '@' = ""
      | inst == ' ' = doInterpret gen code (moveNext code state)

interpret :: StdGen -> String -> String
interpret gen code = doInterpret gen (lines code) (IntState False [] (0, 0) Right)

main :: IO ()
main = do
  g <- newStdGen
  -- print $ interpret g ">987v>.v\nv456<  :\n>321 ^ _@"
  -- print $ interpret g ">                          v\n@,,,,,,,,,,,,\"Hello World!\"<"
  -- print $ interpret g "8>:1-:v v *_$.@ \n ^    _$>\\:^"
  print $ interpret g "2>:3g\" \"-!v\\  g30          <\n |!`\"O\":+1_:.:03p>03g+:\"O\"`|\n @               ^  p3\\\" \":<\n2 234567890123456789012345678901234567890123456789012345678901234567890123456789"
