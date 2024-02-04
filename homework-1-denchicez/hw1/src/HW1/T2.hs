module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z second = second
nplus (S first) second = S (nplus first second)

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult (S Z) second = second
nmult (S first) second = nplus second (nmult first second)

nsub :: N -> N -> Maybe N
nsub first Z = Just first
nsub Z _ = Nothing -- negative!
nsub (S first) (S second) = nsub first second

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp _ Z = GT
ncmp Z _ = LT
ncmp (S first) (S second) = ncmp first second

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S (nFromNatural (x - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S x) = 1 + nToNum x

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S x)) = nEven x

nOdd :: N -> Bool
nOdd x = not(nEven x)

ndiv :: N -> N -> N
ndiv Z _ = Z
ndiv _ Z = Z
ndiv first second = case (nsub first second) of
    Just value -> S (ndiv value second)
    Nothing -> Z

nmod :: N -> N -> N
nmod first Z = first
nmod first second = case (nsub first second) of
    Just value -> nmod value second
    Nothing -> first