{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left val) = (Left val, Left val)
distrib (Right (left, right)) = (Right left, Right right)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso assocEitherLeft assocEitherRight where
  assocEitherLeft :: Either a (Either b c) -> Either (Either a b) c
  assocEitherLeft (Left a) = Left (Left a)
  assocEitherLeft (Right (Left b)) = Left (Right b)
  assocEitherLeft (Right (Right c)) = Right c

  assocEitherRight :: Either (Either a b) c -> Either a (Either b c)
  assocEitherRight (Left (Left a)) = Left a
  assocEitherRight (Left (Right b)) = Right (Left b)
  assocEitherRight (Right c) = Right (Right c)