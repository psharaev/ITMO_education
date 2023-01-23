module HW2.T5
  ( EvaluationError (..),
    ExceptState (..),
    eval,
    joinExceptState,
    mapExceptState,
    modifyExceptState,
    throwExceptState,
    wrapExceptState,
  )
where

import qualified Control.Monad
import HW2.T1
import HW2.T4 (Expr (..), Prim (..), calc, evalMonad)

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \x -> mapExcept (mapAnnotated f) $ runES es x

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \s ->
  case runES es s of
    Success (val :# ann) -> runES val ann
    Error e -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState $ fmap f m

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval expr = evalMonad expr $ \evalOperation -> do
  case evalOperation of
    Div _ 0 -> throwExceptState DivideByZero
    _ -> modifyExceptState $ (:) evalOperation
  pure $ calc evalOperation
