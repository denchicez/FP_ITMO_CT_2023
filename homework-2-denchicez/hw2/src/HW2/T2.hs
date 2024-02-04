module HW2.T2
  ( joinWith
  , splitOn
  , foo
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep arr = foldr (f sep) ([] :| []) arr
  where
    f :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    f delimetr element (first :| other) = if element == delimetr
      then [] :| first : other
      else (element : first) :| other

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (start :| arr) = start ++ foldr (f sep) [] arr
  where
    f :: a -> [a] -> [a] -> [a]
    f delimetr left right = delimetr : left ++ right

foo :: Either a b -> String
foo (Left _) = "left"
foo (Right _) = "right"
