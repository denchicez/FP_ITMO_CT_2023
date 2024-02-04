-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), lLeft, lRight, lGenerator)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f = Grid . (fmap . fmap) f . unGrid

gUp, gDown :: Grid a -> Grid a
gUp   (Grid g) = Grid (lLeft  g)
gDown (Grid g) = Grid (lRight g)

gLeft, gRight :: Grid a -> Grid a
gLeft  (Grid g) = Grid (fmap lLeft  g)
gRight (Grid g) = Grid (fmap lRight g)

gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp   gDown

instance Comonad Grid where
  extract = extract . extract . unGrid
  duplicate = Grid . fmap gHorizontal . gVertical
