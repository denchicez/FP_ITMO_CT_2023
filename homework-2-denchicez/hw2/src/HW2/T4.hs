module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+
instance Semigroup (ListPlus a) where
  (<>) (Last a) b = a :+ b
  (<>) (a :+ b) c = a :+ b <> c

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (That a) (This b)     = Both b a
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (Both b c) (This a)   = Both (b <> a) c
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (Both b c) (That a)   = Both b (c <> a)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) mempty (DS right)   = DS right
  (<>) (DS left) mempty    = DS left
  (<>) (DS left) (DS right) = DS (left ++ "." ++ right)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F left) (F right) = F (left . right)

instance Monoid (Fun a) where
  mempty = F id