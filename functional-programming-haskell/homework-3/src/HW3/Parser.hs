module HW3.Parser (
    HW3.Parser.parse,
)
where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import qualified Data.ByteString
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Numeric (readHex)
import Text.Megaparsec
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral, scientific, signed, skipBlockComment, skipLineComment, space, symbol)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpr <* eof) ""

type Parser = Parsec Void String

pExpr :: Parser HiExpr
pExpr = makeExprParser pHiExpr binaryTable
  where
    pHiExpr :: Parser HiExpr
    pHiExpr = do
        _ <- skipWhiteSpaces
        res <- pDot =<< pArgs =<< (try pValue <|> pBrackets)
        _ <- skipWhiteSpaces
        return res
    pBrackets :: Parser HiExpr
    pBrackets = do
        _ <- char '('
        res <- pExpr
        _ <- char ')'
        return res

binaryTable :: [[Operator Parser HiExpr]]
binaryTable =
    [
        [ infixL "*" $ applyBinary HiFunMul
        , (InfixL . try) $ applyBinary HiFunDiv <$ char '/' <* notFollowedBy (char '=')
        ]
    ,
        [ infixL "+" $ applyBinary HiFunAdd
        , infixL "-" $ applyBinary HiFunSub
        ]
    ,
        [ infixN "==" $ applyBinary HiFunEquals
        , infixN "/=" $ applyBinary HiFunNotEquals
        , infixN "<=" $ applyBinary HiFunNotGreaterThan
        , infixN ">=" $ applyBinary HiFunNotLessThan
        , infixN "<" $ applyBinary HiFunLessThan
        , infixN ">" $ applyBinary HiFunGreaterThan
        ]
    ,
        [ infixR "&&" $ applyBinary HiFunAnd
        ]
    ,
        [ infixR "||" $ applyBinary HiFunOr
        ]
    ]
  where
    applyBinary :: HiFun -> HiExpr -> HiExpr -> HiExpr
    applyBinary fun a b = HiExprApply (HiExprValue $ HiValueFunction fun) [a, b]

    infixL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    infixL = infixOperator InfixL

    infixR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    infixR = infixOperator InfixR

    infixN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
    infixN = infixOperator InfixN

    infixOperator :: (Parser fun -> operator) -> String -> fun -> operator
    infixOperator op name f = op (f <$ symbol skipWhiteSpaces name)

pArgs :: HiExpr -> Parser HiExpr
pArgs inExpr = try (pDot =<< skipWhiteSpaces *> exprWithArgs) <|> pure inExpr
  where
    exprWithArgs = HiExprApply inExpr <$> pArgsVals '(' pExpr ')'

pArgsVals :: Char -> Parser a -> Char -> Parser [a]
pArgsVals start p end = do
    let separated = p `sepBy` (char ',' <* skipWhiteSpaces)
    between (char start <* skipWhiteSpaces) (char end) separated

pValue :: Parser HiExpr
pValue =
    try pList
        <|> try pDict
        <|> HiExprValue
            <$> ( try pAction
                    <|> try pBytes
                    <|> try pFun
                    <|> try pValueNumber
                    <|> try pBool
                    <|> try pNull
                    <|> try pString
                )

pList :: Parser HiExpr
pList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> pArgsVals '[' pExpr ']'

pDict :: Parser HiExpr
pDict = HiExprDict <$> pArgsVals '{' pDictPair '}'
  where
    pDictPair :: Parser (HiExpr, HiExpr)
    pDictPair = do
        key <- pExpr
        _ <- char ':'
        value <- pExpr
        pure (key, value)

pAction :: Parser HiValue
pAction =
    HiValueAction
        <$> choice
            [ HiActionCwd <$ string "cwd"
            , HiActionNow <$ string "now"
            ]

pBytes :: Parser HiValue
pBytes =
    HiValueBytes . Data.ByteString.pack
        <$> between (string "[#" <* skipWhiteSpaces) (string "#]") (sepEndBy pByte (char ' '))
  where
    pByte :: Parser Word8
    pByte = do
        hexString <- count 2 hexDigitChar
        case readHex hexString of
            [(byte, "")] ->
                if 0 <= byte && byte <= 255
                    then return byte
                    else empty
            _ -> empty

pFun :: Parser HiValue
pFun = HiValueFunction <$> choice (Prelude.map (\x -> x <$ string (show x)) [HiFunDiv ..])

pValueNumber :: Parser HiValue
pValueNumber = HiValueNumber . toRational <$> signed skipWhiteSpaces scientific

pBool :: Parser HiValue
pBool =
    choice
        [ HiValueBool True <$ string "true"
        , HiValueBool False <$ string "false"
        ]

pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

pString :: Parser HiValue
pString = HiValueString . Data.Text.pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

pDot :: HiExpr -> Parser HiExpr
pDot res = do
    list <- optional $ char '.' >> sepBy1 isCharOrNum (char '-')
    pArgs =<< maybe (pRun res) (\l -> pDot $ HiExprApply res [concatValues l]) list
  where
    isCharOrNum = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
    concatValues = HiExprValue . HiValueString . Data.Text.pack . Prelude.foldl1 (\x y -> x ++ "-" ++ y)
    pRun inExpr = do
        run <- optional $ char '!'
        maybe (return inExpr) (const $ pDot $ HiExprRun inExpr) run

skipWhiteSpaces :: Parser ()
skipWhiteSpaces =
    space
        space1
        (skipLineComment "//")
        (skipBlockComment "/*" "*/")
