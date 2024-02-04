module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | Convert return type in annotated by given function
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES (mapAnnotated f . runES state) where
  mapAnnotated :: (a -> b) -> Except e (Annotated s a) -> Except e (Annotated s b)
  mapAnnotated mapFunc (Success (val :# e)) = Success (mapFunc val :# e)
  mapAnnotated _ (Error e) = Error e

-- | Return exceptState by given
wrapExceptState :: a -> ExceptState e s a
wrapExceptState val = ES (\s -> Success (val :# s))

-- | Return state in return type by given state
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState state = ES (getAnnotatedExcept . runES state) where
  getAnnotatedExcept :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
  getAnnotatedExcept (Error e) = Error e
  getAnnotatedExcept (Success (annotatedState :# s2)) = runES annotatedState s2

-- | Modify annotated state by given function
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

-- | Return ExceptState which always returns error
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) left right = joinExceptState (mapExceptState right left)

data EvaluationError = DivideByZero
  deriving Show

-- | Evaluate binary operation
evalBinary :: Expr
           -> Expr
           -> (Double -> Double -> (Double, Prim Double))
           -> ExceptState EvaluationError [Prim Double] Double
evalBinary left right f = do
  valueLeft <- eval left
  valueRight <- eval right
  let (valueCalc, valuePrimCalc) = f valueLeft valueRight
  modifyExceptState (valuePrimCalc :)
  pure valueCalc

-- | Evaluate unary operation
evalUnary :: Expr
          -> (Double -> (Double, Prim Double))
          -> ExceptState EvaluationError [Prim Double] Double
evalUnary val f = do
  valueGet <- eval val
  let (valueCalc, valuePrimCalc) = f valueGet
  modifyExceptState (valuePrimCalc :)
  pure valueCalc

-- | Calculate given expression to ExceptState which contains final result of evaluating an expression,
-- it accumulates a trace of all individual operations
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val val) = wrapExceptState val
eval (Op (Add left right)) = evalBinary left right (\x y -> (x + y, Add x y))
eval (Op (Sub left right)) = evalBinary left right (\x y -> (x - y, Sub x y))
eval (Op (Mul left right)) = evalBinary left right (\x y -> (x * y, Mul x y))
eval (Op (Abs val)) = evalUnary val (\x -> (abs x, Abs x))
eval (Op (Sgn val)) = evalUnary val (\x -> (signum x, Sgn x))
eval (Op (Div left right)) = do
  valueLeft <- eval left
  valueRight <- eval right
  when (valueRight == 0.0) (throwExceptState DivideByZero)
  modifyExceptState (Div valueLeft valueRight :)
  pure (valueLeft / valueRight)
