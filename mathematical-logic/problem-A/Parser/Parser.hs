module Parser.Parser where

import Parser.Lexer
import Parser.Syntax
import Logic.Expression

parseExpression :: String -> Expression
parseExpression = parse . alexScanTokens