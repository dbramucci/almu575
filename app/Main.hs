module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.Trifecta (Result(..))
import System.FilePath.Windows
import Parser (parseFull)
import Html (almuToHtml)
import SymbolLookup (builtIn)

main :: IO ()
main = do
    [file] <- getArgs :: IO [String]
    let resultFile = addExtension (dropExtension file) ".html"
    text <- readFile file
    case almuToHtml builtIn <$> parseFull text of
        Success htm -> B.writeFile resultFile htm
        Failure err -> print err

