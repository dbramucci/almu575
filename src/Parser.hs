{-# LANGUAGE OverloadedStrings #-}
module Parser (parseTerm, parseFull) where

import Control.Applicative
import Control.Monad (when)

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
parseFull = parseString (some parseTerm <* eof <?> "Full Almu575 File") mempty

specialChars :: [Char]
specialChars = [escape, openBrace, closeBrace]

parseTerm :: Parser Term
parseTerm = (do
                try (char escape)
                (parseSpecial <|> parseEnv <|> parseSymbol) <?> "Special character, environment or symbol"
            )  <|> (do
                (Text t) <- parseText <?> "Text"
                when (T.length t == 0) (fail "Found empty text segment") -- TODO: remove false positives at the end of a environment/file
                return (Text t)
                ) <?> "Term"

-- | Parse Environment expecting no escape character to prefix it
parseEnv :: Parser Term
parseEnv = do
    env <- try (parseEnvType <?> "Environment name")
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

-- | Parses special characters
parseSpecial :: Parser Term
parseSpecial = do
    c <- oneOf specialChars
    return . Text . T.pack $ [c]

parseText :: Parser Term 
parseText = Text . T.pack <$> many (noneOf specialChars) <?> "Text, note that \"\\\", \"[\" and \"]\" must be escaped with a \"\\\""