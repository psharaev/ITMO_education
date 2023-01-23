module HW1.T5 where

import GHC.Base (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr f ([] :| [])
  where
    f cur (x :| xs) =
      if cur == sep
        then [] :| (x : xs)
        else (cur : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldr1 (\x xs -> x ++ [sep] ++ xs)
