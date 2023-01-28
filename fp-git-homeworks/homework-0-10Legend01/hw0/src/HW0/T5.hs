{-# LANGUAGE LambdaCase #-}

module HW0.T5
  ( Nat,
    nz,
    ns,
    nplus,
    nmult,
    nFromNatural,
    nToNum,
  )
where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns n f a = f (n f a)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus n1 n2 f a = n1 f (n2 f a)
nmult n1 n2 f = n1 (n2 f)

nFromNatural :: Natural -> Nat a
nFromNatural = \case
  0 -> nz
  n -> ns $ nFromNatural (n - 1)

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
