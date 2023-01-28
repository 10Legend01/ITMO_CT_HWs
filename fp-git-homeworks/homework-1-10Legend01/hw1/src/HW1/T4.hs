module HW1.T4
  ( tfoldr,
    treeToList,
  )
where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f ma t = case t of
  Leaf -> ma
  Branch _ l x r -> tfoldr f (f x (tfoldr f ma r)) l

treeToList :: Tree a -> [a] -- output list is sorted
treeToList = tfoldr (:) []
