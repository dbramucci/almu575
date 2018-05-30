{-# LANGUAGE OverloadedStrings #-}
module Parser (parseTerm, parseFull) where

import Control.Applicative
import Term (EnvType(..), SymbolName(..), Term(..))
import Text.Trifecta
import qualified Data.Text as T
import Data.Char (isSpace)

escape :: Char
escape = '\\'

openBrace :: Char
openBrace = '['

closeBrace :: Char
closeBrace = ']'

parseFull :: String -> Result [Term]
parseFull = parseString (some parseTerm <* eof) mempty

specialChars :: [Char]
specialChars = [escape, openBrace, closeBrace]

parseTerm :: Parser Term
parseTerm = parseSpecial <|> parseText 

-- | Parse Environment expecting no escape character to prefix it
parseEnv :: Parser Term
parseEnv = do
    env <- try parseEnvType
    whiteSpace
    char openBrace
    contains <- case env of
            Math -> (: []) <$> parseMath
            Displaymath -> (: []) <$> parseMath
            _           -> some parseTerm
    char closeBrace
    return $ Env env contains
    where
        parseEnvType :: Parser EnvType
        parseEnvType = do
            name <- some alphaNum
            case name of
                          "title"  -> return Title
                          "author" -> return Author
                          "date" -> return Date
                          "section" -> return Section
                          "subsection" -> return Subsection
                          "code" -> return Code
                          "displaymath" -> return Displaymath
                          "math" -> return Math
                          e      -> fail $ "Unrecognized environment: \"" ++ e ++ "\""



-- | Parses the bodies of a Math environment
parseMath :: Parser Term
parseMath = Text . T.pack <$> some (noneOf specialChars) -- TODO: Handle special chars correctly

-- | Parse a symbol, without an escape character
parseSymbol :: Parser Term
parseSymbol = Symbol . SymbolName . T.pack <$> some alphaNum

-- | Parses expressions beginning with an escape character
parseSpecial :: Parser Term
parseSpecial = do
    try (char escape)
    try (do c <- oneOf specialChars
            return . Text . T.pack $ [c])
      <|> parseEnv <|> parseSymbol

parseText :: Parser Term 
parseText = Text . T.pack <$> some (noneOf specialChars)