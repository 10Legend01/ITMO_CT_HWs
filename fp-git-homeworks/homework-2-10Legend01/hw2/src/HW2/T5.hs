{-# LANGUAGE LambdaCase #-}

module HW2.T5
  ( ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    EvaluationError (..),
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES (mapExcept (mapAnnotated f) . g)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES (extractor . f)
  where
    extractor = \case
      Error e -> Error e
      Success ((ES g) :# s) -> g s

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = \case
  Val x -> pure x
  Op (Add x y) -> bin Add x y (+)
  Op (Sub x y) -> bin Sub x y (-)
  Op (Mul x y) -> bin Mul x y (*)
  Op (Div x y) -> do
    a <- eval x
    b <- eval y
    if b == 0
      then throwExceptState DivideByZero
      else modifyExceptState (Div a b :)
    return (a / b)
  Op (Abs x) -> un Abs x abs
  Op (Sgn x) -> un Sgn x signum
  where
    bin expr x y op = do
      a <- eval x
      b <- eval y
      modifyExceptState (expr a b :)
      pure (op a b)
    un expr x op = do
      a <- eval x
      modifyExceptState (expr a :)
      pure (op a)
