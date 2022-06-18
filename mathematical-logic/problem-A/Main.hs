module Main where

import Parser.Parser
import Logic.Expression

main :: IO ()
main = interact $ printAnswer . calcExpression . parseExpression
--    expression <- getLine
--    putStrLn . show . parseExpression $ expression

--export PATH=$PATH:/Users/pechhenka/.cabal/bin/