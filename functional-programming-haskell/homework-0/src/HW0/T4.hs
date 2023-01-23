module HW0.T4 where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' a = fix (a :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' = fix mapRec
  where
    mapRec :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapRec _ _ [] = []
    mapRec rec f (x : xs) = f x : rec f xs

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib = fix (\_ n -> fibRec 0 1 n)
  where
    fibRec :: Natural -> Natural -> Natural -> Natural
    fibRec a b n =
        case n of
            0 -> a
            1 -> b
            _ -> fibRec b (a + b) (n - 1)

fac :: Natural -> Natural -- computes the factorial
fac = fix rec
  where
    rec :: (Natural -> Natural) -> Natural -> Natural
    rec f x
      | x <= 1 = 1
      | otherwise = x * f (x - 1)
