module Ipv4 where

import Data.Bits
import Data.Word (Word32)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP n = init (helper 0 n)
  where
    helper :: Int -> Word32 -> IPString
    helper 32 n = ""
    helper s n = helper (s + 8) n ++ show ((n `shiftR` s) .&. 0xff) ++ "."
