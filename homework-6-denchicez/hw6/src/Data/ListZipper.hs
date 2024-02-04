-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..),
  lLeft, lRight, lGenerator
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ left cur right) = LZ (fmap f left) (f cur) (fmap f right)

lLeft, lRight :: ListZipper a -> ListZipper a
lLeft  (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft lz = lz

lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz = lz

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

instance Comonad ListZipper where
  extract (LZ _ cur _) = cur
  duplicate = lGenerator lLeft lRight