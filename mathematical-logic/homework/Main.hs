module Homework where

import Data.Void
import Data.Either

-- task 6
-- (a) α→β→α
-- (b) (α→β)→(α→~β)→~α
-- (c) α→~~α
-- (d) (~α∨β)→(α→β)
-- (e) (α&β)→~(~α∨~β)

type Not a = a -> Void 

taskA :: a -> b -> a
taskA x _ = x

taskB :: (a -> b) -> (a -> Not b) -> (Not a)
taskB f g a = g a (f a)

taskC :: a -> Not (Not a)
taskC a f = f a

taskD :: (Either (Not a) b) -> (a -> b)
taskD (Left void) = absurd . void
taskD (Right b) = taskA b

-- (a ^ b) -> (!(!a ∨ !b))
-- (a ^ b) -> (!(a -> ⊥ ∨ b -> ⊥))
-- (a ^ b) -> ((a -> ⊥ ∨ b -> ⊥) -> ⊥)
taskE :: (a, b) -> Not (Either (Not a) (Not b))
taskE (a, _) (Left toVoid) = toVoid a
takeE (_, b) (Right toVoid) = toVoid b

-- task 7
-- (a) α×(β+γ)=α×β+α×γ
-- (b) α^(β+γ)=α^β×α^γ
-- (c) α^(β×γ)=(α^β)^γ

taskALR :: (a, Either b c) -> Either (a, b) (a, c)
taskALR (a, Left  b) = Left  (a, b)
taskALR (a, Right c) = Right (a, c)

taskARL :: Either (a, b) (a, c) -> (a, Either b c)
taskARL (Left  (a, b)) = (a, Left  b)
taskARL (Right (a, c)) = (a, Right c)

taskBLR :: ((Either b c) -> a) -> ((b -> a), (c -> a))
taskBLR f = ((\b -> f $ Left  b),
             (\c -> f $ Right c))

taskBRL :: ((b -> a), (c -> a)) -> ((Either b c) -> a)
taskBRL (bf, cf) = f
  where
    f (Left  b) = bf b
    f (Right c) = cf c

taskCLR :: ((b, c) -> a) -> b -> c -> a -- curry
taskCLR f b c = f (b, c)

taskCRL :: (b -> c -> a) -> (b, c) -> a -- uncurry
taskCRL f (b, c) = f b c