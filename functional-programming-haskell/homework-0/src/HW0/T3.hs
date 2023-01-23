module HW0.T3 where

s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k a _ = a

i :: a -> a
i a = k a a

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

contract :: (a -> a -> b) -> (a -> b)
contract = s s $ s k

permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k (s (k s) k)) s) (k k)
