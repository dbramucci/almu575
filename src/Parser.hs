{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Parser (parseTerm) where


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

specialChars :: [Char]
specialChars = [escape, openBrace, closeBrace]

parseTerm :: Parser Term
parseTerm = parseSpecial <|> parseEnv

-- | Parse Environment expecting an escape character to prefix it
parseEnv :: Parser Term
parseEnv = do
    env <- try parseEnvType
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
            return $ case name of
                          "title"  -> Title
                          "author" -> Author
                          "date" -> Date
                          "section" -> Section
                          "subsection" -> Subsection
                          "code" -> Code
                          "displaymath" -> Displaymath
                          "math" -> Math



-- | Parses the bodies of a Math environment
parseMath :: Parser Term
parseMath = Text <$> some (noneOf specialChars) -- TODO: Handle special chars correctly

-- | Parse a symbol, expecting an escape character to prefix it
parseSymbol :: Parser Term
parseSymbol = Symbol . SymbolName <$> some alphaNum

-- | Parses a special character, expecting an escape character first
parseSpecial :: Parser Term
parseSpecial = do
    try (char escape)
    c <- oneOf specialChars
    if c `elem` specialChars
    then return $ Text [escape]
    else parseEnv <|> parseSymbol

parseText :: Parser Term 
parseText = Text <$> some (noneOf specialChars)