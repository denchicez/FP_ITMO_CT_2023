module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat arr = foldr f mempty arr
  where
    f :: Monoid a => Maybe a -> a -> a
    f (Just newValue) value = newValue <> value
    f Nothing value         = value

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart arr = foldr f (mempty, mempty) arr
  where
    f :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    f (Left left) (leftArr, rightArr)   = (left <> leftArr, rightArr)
    f (Right right) (leftArr, rightArr) = (leftArr, right <> rightArr)
