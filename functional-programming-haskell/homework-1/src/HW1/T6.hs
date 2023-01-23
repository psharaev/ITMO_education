module HW1.T6 where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr f (mempty, mempty)
  where
    f (Left left) (a, b)   = (left <> a, b)
    f (Right right) (a, b) = (a, right <> b)
