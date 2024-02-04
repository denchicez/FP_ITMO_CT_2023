module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some val) = val
joinOption None = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error err) = Error err
joinExcept (Success val) = val

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((val :# errFirst) :# errSecond) = val :# (errSecond <> errFirst)

joinList :: List (List a) -> List a
joinList (x :. xs) = concatList x where
  concatList (xOther :. xsOther) =  xOther :. concatList xsOther
  concatList _ = joinList xs
joinList _ = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun f = F (\i -> funGet(funGet(f, i), i)) where
  funGet :: (Fun i a, i) -> a
  funGet (F g, arg) = g arg
