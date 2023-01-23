module HW1.T2 where

import Numeric.Natural (Natural)

data N = Z | S N

nplus :: N -> N -> N -- addition
nplus a Z = a
nplus Z b = b
nplus a (S b) = S $ nplus a b

nmult :: N -> N -> N -- multiplication
nmult Z _ = Z
nmult _ Z = Z
nmult a (S Z) = a
nmult (S Z) b = b
nmult a (S b) = nplus a $ nmult a b

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub Z (S _) = Nothing
nsub a Z = Just a
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp (S _) Z = GT
ncmp Z (S _) = LT
ncmp Z Z = EQ
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural 1 = S Z
nFromNatural x = S $ nFromNatural (x - 1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S a) = 1 + nToNum a

nEven :: N -> Bool -- parity checking
nEven Z = True
nEven (S Z) = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool -- parity checking
nOdd Z = False
nOdd (S Z) = True
nOdd (S (S a)) = nOdd a

ndiv :: N -> N -> N -- integer division
ndiv a = ndivImpl (Just a)

ndivImpl :: Maybe N -> N -> N
ndivImpl Nothing _ = error "division by zero, you're a bad boy, I'm formatting the C disk"
ndivImpl (Just a) b = case ncmp a b of
  LT -> Z
  EQ -> S Z
  GT -> S $ ndivImpl (nsub a b) b

nmod :: N -> N -> N -- modulo operation
nmod a = nmodImpl (Just a)

nmodImpl :: Maybe N -> N -> N
nmodImpl Nothing _ = error "division by zero, you're a bad boy, I'm formatting the C disk"
nmodImpl (Just a) b = case ncmp a b of
  LT -> a
  EQ -> Z
  GT -> nmodImpl (nsub a b) b
