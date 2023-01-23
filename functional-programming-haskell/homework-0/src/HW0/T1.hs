{-# LANGUAGE TypeOperators #-}

module HW0.T1 where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEitherToRight :: Either a (Either b c) -> Either (Either a b) c
assocEitherToRight (Left a) = Left $ Left a
assocEitherToRight (Right (Left b)) = Left $ Right b
assocEitherToRight (Right (Right c)) = Right c

assocEitherToLeft :: Either (Either a b) c -> Either a (Either b c)
assocEitherToLeft (Right c) = Right $ Right c
assocEitherToLeft (Left (Right b)) = Right $ Left b
assocEitherToLeft (Left (Left a)) = Left a

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso assocEitherToRight assocEitherToLeft
