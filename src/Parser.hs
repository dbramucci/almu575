{-# LANGUAGE OverloadedStrings #-}
module Parser (parseTerm) where


import Control.Applicative
import Term (EnvType, SymbolName, Term)
import Text.Trifecta
import qualified Data.Text as T

openBrace :: T.Text
openBrace = "["

closeBrace :: T.Text
closeBrace = "]"

specialChars = ["\\", "[", "]"]

parseTerm :: Parser Term
parseTerm = do undefined

parseBackslash :: Parser Term
parseBackslash = do
    c <- letter
    undefined

-- parseText