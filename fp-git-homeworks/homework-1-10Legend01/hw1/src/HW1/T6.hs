module HW1.T6
  ( mcat,
    epart,
  )
where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat array = fold (fold array)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl f (mempty, mempty)
  where
    f (left, right) x = case x of
      Left a -> (left <> a, right)
      Right a -> (left, right <> a) 
