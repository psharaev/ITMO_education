{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..),
    Parser (..),
    pChar,
    pEof,
    parseError,
    parseExpr,
    runP,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus, mfilter)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (ES, runES))

newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P p) s =
  case runES p (0, s) of
    Success (val :# _) -> Success val
    Error err -> Error err

pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES (\(pos, _) -> Error $ ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P parserA) <|> (P parserB) = P $
    ES $ \d ->
      case runES parserA d of
        Error _ -> runES parserB d
        success -> success

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _ -> Error (ErrorAtPos pos)

-- | LL(1)
-- Expr -> E
--
-- E  -> TE'
-- E' -> +TE'
-- E' -> -TE'
-- E' -> ε
-- T  -> FT'
-- T' -> *FT'
-- T' -> /FT'
-- T' -> ε
-- F  -> double number
-- F  -> (Expr)
parseExpr :: String -> Except ParseError Expr
parseExpr = runP (pExpr <* pWhiteSpace <* pEof)

pExpr :: Parser Expr
pExpr = pE

pE :: Parser Expr
pE = liftA2 (\a f -> f a) pT pE'

pE' :: Parser (Expr -> Expr)
pE' = pWhiteSpace *> (rest <|> pure id)
  where
    rest = liftA3 concatExpression (filterChar '+' <|> filterChar '-') pT pE'

pT :: Parser Expr
pT = liftA2 (\a f -> f a) pF pT'

pT' :: Parser (Expr -> Expr)
pT' = pWhiteSpace *> (rest <|> pure id)
  where
    rest = liftA3 concatExpression (filterChar '*' <|> filterChar '/') pF pT'

pF :: Parser Expr
pF = pWhiteSpace *> (number <|> brackets)
  where
    number = Val <$> pDouble
    brackets = (filterChar '(' *> pExpr) <* pWhiteSpace <* filterChar ')'

pDouble :: Parser Double
pDouble = pWhiteSpace *> liftA2 concatDouble pIntegerAsString pAdditional

pAdditional :: Parser String
pAdditional = (filterChar '.' *> pIntegerAsString) <|> pure []

pIntegerAsString :: Parser String
pIntegerAsString = some $ filterFun isDigit

concatDouble :: String -> String -> Double
concatDouble beforeDot afterDot = toRealFloat $ scientific number degree
  where
    number = toInteger $ foldl concatIntChar 0 $ beforeDot <> afterDot
    degree = - (length afterDot)

concatIntChar :: Int -> Char -> Int
concatIntChar x c = x * 10 + digitToInt c

concatExpression :: Char -> Expr -> (Expr -> Expr) -> Expr -> Expr
concatExpression op first second prev = second $
  case op of
    '+' -> Op (Add prev first)
    '-' -> Op (Sub prev first)
    '*' -> Op (Mul prev first)
    '/' -> Op (Div prev first)
    _ -> error "Undefined operator"

filterChar :: Char -> Parser Char
filterChar c = filterFun (== c)

filterFun :: (Char -> Bool) -> Parser Char
filterFun f = mfilter f pChar

pWhiteSpace :: Parser String
pWhiteSpace = many $ filterFun isSpace
