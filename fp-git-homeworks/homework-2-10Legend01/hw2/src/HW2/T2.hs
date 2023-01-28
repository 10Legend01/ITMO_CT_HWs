{-# LANGUAGE LambdaCase #-}

module HW2.T2
  ( distOption,
    wrapOption,
    distPair,
    wrapPair,
    distQuad,
    wrapQuad,
    distAnnotated,
    wrapAnnotated,
    distExcept,
    wrapExcept,
    distPrioritised,
    wrapPrioritised,
    distStream,
    wrapStream,
    mergeLists,
    distList,
    wrapList,
    distFun,
    wrapFun,
  )
where

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption = \case
  (None, _) -> None
  (_, None) -> None
  (Some a, Some b) -> Some (a, b)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad = \case
  (Q a b c d, Q k l m n) -> Q (a, k) (b, l) (c, m) (d, n)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept = \case
  (Error e, _) -> Error e
  (_, Error e) -> Error e
  (Success a, Success b) -> Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised = \case
  (Low a, Low b) -> Low (a, b)
  (Low a, Medium b) -> Medium (a, b)
  (Low a, High b) -> High (a, b)
  (Medium a, Low b) -> Medium (a, b)
  (Medium a, Medium b) -> Medium (a, b)
  (Medium a, High b) -> High (a, b)
  (High a, Low b) -> High (a, b)
  (High a, Medium b) -> High (a, b)
  (High a, High b) -> High (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> x, b :> y) = (a, b) :> distStream (x, y)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

mergeLists :: (List a, List a) -> List a
mergeLists = \case
  (Nil, r) -> r
  (a :. x, r) -> a :. mergeLists (x, r)

distList :: (List a, List b) -> List (a, b)
distList = \case
  (Nil, _) -> Nil
  (_, Nil) -> Nil
  (a :. x, r) -> mergeLists (enum a r, distList (x, r))
  where
    enum :: a -> List b -> List (a, b)
    enum e = \case
      Nil -> Nil
      (h :. t) -> (e, h) :. enum e t

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\x -> (f1 x, f2 x))

wrapFun :: a -> Fun i a
wrapFun a = F (const a)
