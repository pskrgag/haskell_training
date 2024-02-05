{-# LANGUAGE NoImplicitPrelude #-}

module Monads where

import Data.Monoid
import Prelude hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a}

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return a = Identity a
  (Identity v) >>= f = f v

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return a = State (a,)
  (State g) >>= f = State $ \s -> let (out, new_s) = g s in (runState $ f out) new_s

instance Monad (Reader s) where
  return a = Reader $ const a
  (Reader g) >>= f = Reader $ \s -> let new_a = g s in (runReader $ f new_a) s

-- Writer monad does an operation and appends longs
instance (Monoid w) => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (s, v)) >>= f = Writer $ let Writer (s', v') = f v in (mappend s s', v')
