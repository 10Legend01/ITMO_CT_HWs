{-# LANGUAGE LambdaCase #-}

module HW2.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW2.T1
import HW2.T2 (mergeLists)

joinOption :: Option (Option a) -> Option a
joinOption = \case
  None -> None
  Some a -> a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept = \case
  Error e -> Error e
  Success a -> a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# e1 <> e2

joinList :: List (List a) -> List a
joinList = \case
  Nil -> Nil
  a :. Nil -> a
  a :. t -> mergeLists (a, joinList t)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> (\(F a) -> a i) (f i))
