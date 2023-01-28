{-# LANGUAGE BlockArguments #-}

module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

type Meta = (Int, Int)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize t = case t of
  Leaf -> 0
  Branch (n, _) _ _ _ -> n

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth t = case t of
  Leaf -> 0
  Branch (_, d) _ _ _ -> d

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember a t = case t of
  Branch _ l x r
    | a < x -> tmember a l
    | x == a -> True
    | x < a -> tmember a r
  _ -> False

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch (n, d) l x r
  where
    n = 1 + tsize l + tsize r
    d = 1 + max (tdepth l) (tdepth r)

bfactor :: Tree a -> Int
bfactor t = case t of
  Leaf -> 0
  Branch _ l _ r -> tdepth r - tdepth l

rotateRight :: Tree a -> Tree a
rotateRight t = case t of
  Branch _ (Branch _ a x b) y c -> mkBranch a x (mkBranch b y c)
  _ -> t

rotateLeft :: Tree a -> Tree a
rotateLeft t = case t of
  Branch _ a x (Branch _ b y c) -> mkBranch (mkBranch a x b) y c
  _ -> t

-- | https://habr.com/ru/post/150732/
tBalance :: Tree a -> Tree a
tBalance tree = case tree of
  Leaf -> tree
  Branch _ l x r
    | bfactor tree == 2 ->
      if bfactor r < 0
        then rotateLeft (mkBranch l x (rotateRight r))
        else rotateLeft tree
    | bfactor tree == -2 ->
      if bfactor l > 0
        then rotateRight (mkBranch (rotateLeft l) x r)
        else rotateRight tree
    | otherwise -> tree

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a t = case t of
  Branch _ l x r
    | a < x -> tBalance $ mkBranch (tinsert a l) x r
    | a == x -> t
    | x < a -> tBalance $ mkBranch l x (tinsert a r)
  _ -> mkBranch Leaf a Leaf

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList list
  | null list = Leaf
  | otherwise = helper list Leaf
  where
    helper ma t = case ma of
      [] -> t
      a : ma' -> helper ma' (tinsert a t)
