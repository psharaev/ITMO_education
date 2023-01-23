module HW1.T4 where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ l x r) = tfoldr f (f x $ tfoldr f acc r) l

treeToList :: Tree a -> [a] -- output list is sorted
treeToList = tfoldr (:) []
