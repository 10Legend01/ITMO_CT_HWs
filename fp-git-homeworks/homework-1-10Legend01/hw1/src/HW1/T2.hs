{-# LANGUAGE LambdaCase #-}

module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import GHC.Natural (Natural)

data N = Z | S N
  deriving (Show)

nplus :: N -> N -> N -- addition
nplus a b = case (a, b) of
  (Z, _) -> b
  (_, Z) -> a
  (S x, S y) -> S $ S (nplus x y)

nmult :: N -> N -> N -- multiplication
nmult a b = case a of
  Z -> Z
  S Z -> b
  S x -> nplus (nmult x b) b

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub a b = case (a, b) of
  (_, Z) -> Just a
  (Z, S _) -> Nothing
  (S x, S y) -> nsub x y

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp a b = case (a, b) of
  (Z, Z) -> EQ
  (Z, _) -> LT
  (_, Z) -> GT
  (S x, S y) -> ncmp x y

nFromNatural :: Natural -> N
nFromNatural = \case
  0 -> Z
  n -> S (nFromNatural (n -1))

nToNum :: Num a => N -> a
nToNum = \case
  Z -> 0
  S x -> 1 + nToNum x

nEven, nOdd :: N -> Bool -- parity checking
nEven = \case
  Z -> True
  S Z -> False
  S (S x) -> nEven x
nOdd a = not $ nEven a

ndiv :: N -> N -> N -- integer division
ndiv a b = case nsub a b of
  Nothing -> Z
  Just Z -> S Z
  Just x -> S $ ndiv x b

nmod :: N -> N -> N -- modulo operation
nmod a b = case nsub a b of
  Nothing -> a
  Just Z -> Z
  Just x -> nmod x b
