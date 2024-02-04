module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec arr' -> case arr' of
               (x: xs) -> f x : rec xs
               [] -> [] )

fib :: Natural -> Natural
fib = fix (\rec x y n -> if n < 1 then y else rec y (x+y) (n-1)) 1 0

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n-1))
