module HW1.T3 where

import Data.Foldable (foldr')

type Meta = Int -- (size, depth)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

-- 1. Sorted: The elements in the left subtree are less than the head element of a branch, and the elements in the right subtree are greater.
-- 2. Unique: There are no duplicate elements in the tree (follows from Sorted).
-- 3. CachedSize: The size of the tree is cached in the Meta field for constant-time access.
-- 4. (Advanced) Balanced: The tree is balanced according to one of the following strategies:
--    * SizeBalanced: For any given Branch _ l _ r, the ratio between the size of l and the size of r never exceeds 3.
--    * HeightBalanced: For any given Branch _ l _ r, the difference between the height of l and the height of r never exceeds 1.

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch size _ _ _) = size

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ l _ r) = max (tdepth l) (tdepth r) + 1

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember key (Branch _ l a r)
  | key == a = True
  | key < a = tmember key l
  | otherwise = tmember key r

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert key Leaf = mkBranch Leaf key Leaf
tinsert key tree@(Branch _ l x r)
  | key < x = mkBranch (tinsert key l) x r
  | key > x = mkBranch l x (tinsert key r)
  | otherwise = tree

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

-- | in order to maintain the CachedSize invariant, define a helper function:
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch Leaf value Leaf = Branch 1 Leaf value Leaf
mkBranch Leaf value right = Branch (tsize right + 1) Leaf value right
mkBranch left value Leaf = Branch (tsize left + 1) left value Leaf
mkBranch left value right = Branch (tsize left + tsize right + 1) left value right
