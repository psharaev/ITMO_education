module Logic.Expression where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Operation = And
               | Impl
               | Or
               | Not

data Expression = Binary Operation Expression Expression
                | Unary Operation Expression
                | Var String

class Calculate a where
    eval :: a -> Map String Bool -> Bool

instance Calculate Expression where
    eval (Binary And l r)  m = eval l m && eval r m
    eval (Binary Or l r)   m = eval l m || eval r m
    eval (Binary Impl l r) m = not (eval l m) || eval r m
    eval (Unary Not e)     m = not $ eval e m
    eval (Var name)        m = m Map.! name

collectVars :: Expression -> Set String
collectVars (Var name)     = Set.singleton name
collectVars (Unary _ e)    = collectVars e
collectVars (Binary _ l r) = Set.union (collectVars l) (collectVars r)

toBitsBySize :: Int -> Int -> [Bool]
toBitsBySize  0 x = []
toBitsBySize sz 0 = [False | i <- [1..sz]]
toBitsBySize sz x = if k == 0
                    then False : (toBitsBySize n x)
                    else True  : (toBitsBySize n (x - k*m))
                    where n = sz - 1
                          m = 2 ^ n
                          k = x `div` m

merge :: [a] -> [b] -> [(a, b)]
merge (x:xs) (y:ys) = (x, y) : merge xs ys
merge [] [] = []

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

sumBool :: Int -> Bool -> Int
sumBool a b = a + toInt b

calcExpression :: Expression -> (Int, Int)
calcExpression e = (t, f)
    where s = collectVars e
          n = Set.size s
          t = foldl sumBool 0 (map (\k -> eval e $ Map.fromList $ merge (Set.toList s) $ toBitsBySize n k) [0..(2^n - 1)])
          f = 2^n - t

printAnswer :: (Int, Int) -> String
printAnswer (_, 0) = "Valid"
printAnswer (0, _) = "Unsatisfiable"
printAnswer (t, f) = printf "Satisfiable and invalid, %d true and %d false cases" t f