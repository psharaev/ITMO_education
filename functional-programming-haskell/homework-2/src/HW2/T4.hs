module HW2.T4
  ( Expr (..),
    Prim (..),
    State (..),
    eval,
    joinState,
    mapState,
    modifyState,
    wrapState,
    calc,
    evalMonad,
  )
where

import qualified Control.Monad
import HW2.T1

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f s = S $ \x -> mapAnnotated f $ runS s x

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState s1 = S $ \x -> let a :# s = runS s1 x in runS a s

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val $ fromRational x

eval :: Expr -> State [Prim Double] Double
eval expr = evalMonad expr $ \evalOperation -> do
  modifyState $ (:) evalOperation
  pure $ calc evalOperation

calc :: (Fractional f) => Prim f -> f
calc op = case op of
  Add a b -> a + b
  Sub a b -> a - b
  Mul a b -> a * b
  Div a b -> a / b
  Abs a -> abs a
  Sgn a -> signum a

evalMonad :: (Monad m, Fractional f) => Expr -> (Prim f -> m f) -> m f
evalMonad (Val a) _ = pure $ realToFrac a
evalMonad (Op op) f = case op of
  Add a b -> binary Add a b f
  Sub a b -> binary Sub a b f
  Mul a b -> binary Mul a b f
  Div a b -> binary Div a b f
  Abs a -> unary Abs a f
  Sgn a -> unary Sgn a f

unary :: (Monad m, Fractional f) => (f -> Prim f) -> Expr -> (Prim f -> m f) -> m f
unary toPrim x toMonad = evalMonad x toMonad >>= toMonad . toPrim

binary :: (Monad m, Fractional f) => (f -> f -> Prim f) -> Expr -> Expr -> (Prim f -> m f) -> m f
binary toPrim x y toMonad = do
  evalX <- evalMonad x toMonad
  evalY <- evalMonad y toMonad
  toMonad (toPrim evalX evalY)
