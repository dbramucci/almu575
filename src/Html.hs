module Html where

import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import qualified Data.ByteString.Lazy as B
import Control.Monad (mapM_)

import Term

almuToHtml' :: Term -> Html
almuToHtml' (Text t) = toHtml t
almuToHtml' (Symbol (SymbolName t)) = b $ toHtml t
almuToHtml' (Env Title ts) = h1 $ mapM_ almuToHtml' ts
almuToHtml' (Env Author ts) = h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Date ts) = h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Section ts) = h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Subsection ts) = h3 $ mapM_ almuToHtml' ts
almuToHtml' (Env Code ts) = code $ mapM_ almuToHtml' ts
almuToHtml' (Env Displaymath ts) = error "Sorry no math support yet"
almuToHtml' (Env Math ts) = error "Sorry no math support yet"

almuToHtml :: [Term] -> B.ByteString
almuToHtml ts = renderMarkup $ docTypeHtml . body $ mapM_ almuToHtml' ts