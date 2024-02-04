module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ base Leaf = base
tfoldr func base (Branch _ left val right) = tfoldr func (func val (tfoldr func base right)) left