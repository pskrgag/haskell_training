module Codewars.Kata.DNA where

import Codewars.Kata.DNA.Types

-- data Base = A | T | G | C
type DNA = [Base]

match :: Base -> Base
match c = case c of
  A -> T
  T -> A
  G -> C
  C -> G

dnaStrand :: DNA -> DNA
dnaStrand = map match
