module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.Trifecta (Result(..))
import System.FilePath.Windows
import Parser (parseFull)
import Html (almuToHtml)

main :: IO ()
main = do
    [file] <- getArgs :: IO [String]
    let resultFile = addExtension (dropExtension file) ".html"
    text <- readFile file
    case almuToHtml <$> parseFull text of
        Success htm -> B.writeFile resultFile htm
        Failure err -> print err

