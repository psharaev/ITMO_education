module HW2.T1
  ( Annotated (..),
    Except (..),
    Fun (..),
    List (..),
    Option (..),
    Pair (..),
    Prioritised (..),
    Quad (..),
    Stream (..),
    Tree (..),
    mapAnnotated,
    mapExcept,
    mapFun,
    mapList,
    mapOption,
    mapPair,
    mapPrioritised,
    mapQuad,
    mapStream,
    mapTree,
  )
where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f (Some a) = Some $ f a
mapOption _ _ = None

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = Success $ f a
mapExcept _ (Error e) = Error e

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low $ f a
mapPrioritised f (Medium a) = Medium $ f a
mapPrioritised f (High a) = High $ f a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (v :> rest) = f v :> mapStream f rest

mapList :: (a -> b) -> (List a -> List b)
mapList f (a :. rest) = f a :. mapList f rest
mapList _ Nil = Nil

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Branch l v r) = Branch (mapTree f l) (f v) (mapTree f r)
mapTree _ Leaf = Leaf
