module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S state1) = S (mapAnnotated f . state1)

wrapState :: a -> State s a
wrapState value = S (value :#)

stateGet :: (State s a, s) -> Annotated s a
stateGet (state, arg) = runS state arg

joinState :: State s (State s a) -> State s a
joinState state = S (\s -> stateGet (annotatedGet (stateGet (state, s)))) where
  annotatedGet :: Annotated s a -> (a, s)
  annotatedGet (val :# s) = (val, s)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) converter val = S (\s -> mapAnnotated (annotatedGetValue (stateGet (converter, s))) (stateGet (val, s))) where
    annotatedGetValue :: Annotated s a -> a
    annotatedGetValue (value :# _) = value

instance Monad (State s) where
  (>>=) left right = joinState (mapState right left)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) left right = Op (Add left right)
  (-) left right = Op (Sub left right)
  (*) left right = Op (Mul left right)
  abs val = Op (Abs val)
  signum val = Op (Sgn val)
  fromInteger val = Val (fromInteger val)

instance Fractional Expr where
  (/) left right = Op (Div left right)
  fromRational val = Val (fromRational val)

eval :: Expr -> State [Prim Double] Double

evalBinary :: Expr -> Expr -> (Double -> Double -> (Double, Prim Double)) -> State [Prim Double] Double
evalBinary left right f = do
  valueLeft <- eval left
  valueRight <- eval right
  let (valueCalc, valuePrimCalc) = f valueLeft valueRight
  modifyState (valuePrimCalc :)
  pure valueCalc

evalUnary :: Expr -> (Double -> (Double, Prim Double)) -> State [Prim Double] Double
evalUnary val f = do
  valueGet <- eval val
  let (valueCalc, valuePrimCalc) = f valueGet
  modifyState (valuePrimCalc :)
  pure valueCalc

eval (Val val) = wrapState val
eval (Op (Add left right)) = evalBinary left right (\x y -> (x + y, Add x y))
eval (Op (Sub left right)) = evalBinary left right (\x y -> (x - y, Sub x y))
eval (Op (Mul left right)) = evalBinary left right (\x y -> (x * y, Mul x y))
eval (Op (Div left right)) = evalBinary left right (\x y -> (x / y, Div x y))
eval (Op (Abs val)) = evalUnary val (\x -> (abs x, Abs x))
eval (Op (Sgn val)) = evalUnary val (\x -> (signum x, Sgn x))
