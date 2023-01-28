{-# LANGUAGE LambdaCase #-}

module HW2.T4
  ( State (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    Prim (..),
    Expr (..),
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (mapAnnotated f . g)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S ((\((S g) :# s) -> g s) . f)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval = \case
  Val x -> pure x
  Op (Add x y) -> bin Add x y (+)
  Op (Sub x y) -> bin Sub x y (-)
  Op (Mul x y) -> bin Mul x y (*)
  Op (Div x y) -> bin Div x y (/)
  Op (Abs x) -> un Abs x abs
  Op (Sgn x) -> un Sgn x signum
  where
    bin expr x y op = do
      a <- eval x
      b <- eval y
      modifyState (expr a b :)
      pure (op a b)
    un expr x op = do
      a <- eval x
      modifyState (expr a :)
      pure (op a)
