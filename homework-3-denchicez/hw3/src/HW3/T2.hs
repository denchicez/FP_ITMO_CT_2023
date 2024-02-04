module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some left, Some right) = Some (left, right)
distOption _ = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P firstLeft firstRight, P secondLeft secondRight) = P (firstLeft, secondLeft) (firstRight, secondRight)

wrapPair :: a -> Pair a
wrapPair val = P val val

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 b1 c1 d1, Q a2 b2 c2 d2) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2)

wrapQuad :: a -> Quad a
wrapQuad val = Q val val val val

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (valFirst :# err1, valSecond :# err2) = (valFirst, valSecond) :# (err1 <> err2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated val = val :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error err, _) = Error err
distExcept (_, Error err) = Error err
distExcept (Success first, Success second) = Success (first, second)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low left, Low right) = Low (left, right)
distPrioritised (Low left, Medium right) = Medium (left, right)
distPrioritised (Low left, High right) = High (left, right)
distPrioritised (Medium left, Low right) = Medium (left, right)
distPrioritised (Medium left, Medium right) = Medium (left, right)
distPrioritised (Medium left, High right) = High (left, right)
distPrioritised (High left, Low right) = High (left, right)
distPrioritised (High left, Medium right) = High (left, right)
distPrioritised (High left, High right) = High (left, right)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x1 :> xs1, x2 :> xs2) = (x1, x2) :> distStream (xs1, xs2)

wrapStream :: a -> Stream a
wrapStream val = val :> wrapStream val

distList :: (List a, List b) -> List (a, b)
distList (x1 :. xs1, xs2) = f xs2 where
  f (x2 :. xs2Tail) = (x1, x2) :. f xs2Tail
  f _ = distList (xs1, xs2)
distList _ = Nil

wrapList :: a -> List a
wrapList val = val :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F left, F right) = F (\i -> (left i, right i))

wrapFun :: a -> Fun i a
wrapFun val = F (const val)
