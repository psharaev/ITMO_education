module HW0.T5 where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns n f a = f $ n f a

nplus :: Nat a -> Nat a -> Nat a
nplus n m f a = n f (m f a)

nmult :: Nat a -> Nat a -> Nat a
nmult n m f = n (m f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = \_ a -> a
nFromNatural x = \f a -> nFromNatural (x - 1) f (f a)

nToNum :: Num a => Nat a -> a
nToNum x = x (+ 1) 0
