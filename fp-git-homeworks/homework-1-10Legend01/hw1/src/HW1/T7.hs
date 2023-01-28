module HW1.T7 where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last a <> ma = a :+ ma
  (a :+ ma') <> ma = a :+ (ma' <> ma)

data Inclusive a b = This a | That b | Both a b
  deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This x) <> (This y) = This (x <> y)
  (This x) <> (That y) = Both x y
  (This x) <> (Both y z) = Both (x <> y) z
  (That x) <> (This y) = Both y x
  (That x) <> (That y) = That (x <> y)
  (That x) <> (Both y z) = Both y (x <> z)
  (Both x y) <> (This z) = Both (x <> z) y
  (Both x y) <> (That z) = Both x (y <> z)
  (Both x y) <> (Both x' y') = Both (x <> x') (y <> y')

--This i  <>  This j  =  This (i <> j)   -- OK
--This i  <>  This _  =  This i          -- This is not the Semigroup you're looking for.

newtype DotString = DS String
  deriving (Show)

instance Monoid DotString where
  mempty = DS ""

instance Semigroup DotString where
  x <> (DS "") = x
  (DS "") <> y = y
  (DS x) <> (DS y) = DS $ x ++ "." ++ y

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F f) <> (F g) = F $ f . g

instance Monoid (Fun a) where
  mempty = F id
