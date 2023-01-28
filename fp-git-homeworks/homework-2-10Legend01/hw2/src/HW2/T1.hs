{-# LANGUAGE LambdaCase #-}

module HW2.T1
  ( Option (..),
    mapOption,
    Pair (..),
    mapPair,
    Quad (..),
    mapQuad,
    Annotated (..),
    mapAnnotated,
    Except (..),
    mapExcept,
    Prioritised (..),
    mapPrioritised,
    Stream (..),
    mapStream,
    List (..),
    mapList,
    Fun (..),
    mapFun,
    Tree (..),
    mapTree,
  )
where

data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = \case
  None -> None
  Some a -> Some (f a)

data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f = \case
  P a b -> P (f a) (f b)

data Quad a = Q a a a a

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f = \case
  Q a b c d -> Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e
  deriving (Show)

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f = \case
  a :# e -> f a :# e

data Except e a = Error e | Success a
  deriving (Show)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f = \case
  Error e -> Error e
  Success a -> Success (f a)

data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f = \case
  Low a -> Low (f a)
  Medium a -> Medium (f a)
  High a -> High (f a)

data Stream a = a :> Stream a

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f = \case
  a :> b -> f a :> mapStream f b

data List a = Nil | a :. List a
  deriving (Show)

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList f = \case
  Nil -> Nil
  a :. b -> f a :. mapList f b

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f = \case
  F g -> F (f . g)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f = \case
  Leaf -> Leaf
  Branch l a r -> Branch (mapTree f l) (f a) (mapTree f r)
