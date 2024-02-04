module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ val = val

ns :: Nat a -> Nat a
ns n f x = f (n f x)

nplus :: Nat a -> Nat a -> Nat a
nplus first second f x = first f $ second f x

nmult :: Nat a -> Nat a -> Nat a
nmult first second f = first (second f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural val = ns (nFromNatural (val - 1))

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0