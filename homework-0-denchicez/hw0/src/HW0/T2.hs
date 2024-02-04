module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg val f = f val

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f val = f (doubleNeg val)
