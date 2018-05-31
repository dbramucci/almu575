{-# LANGUAGE OverloadedStrings #-}

module Html where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Text.Blaze
import Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Att
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import qualified Data.ByteString.Lazy as B
import Control.Monad (mapM_)

import Term

center :: Html -> Html
center = Html5.div ! Att.style "text-align:center;"

almuToHtml' :: M.Map SymbolName T.Text -> Term -> Html 
almuToHtml' _ (Text t) = toHtml t
almuToHtml' symbs (Symbol s) = toHtml $ symbs M.! s
almuToHtml' symbs (Env Title ts) = center . h1 $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Author ts) = center . h2 $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Date ts) = center . h2 $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Section ts) = h2 $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Subsection ts) = h3 $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Code ts) = pre $ mapM_ (almuToHtml' symbs) ts
almuToHtml' symbs (Env Displaymath ts) = error "Sorry no math support yet"
almuToHtml' symbs (Env Math ts) = error "Sorry no math support yet"

almuToHtml :: M.Map SymbolName T.Text -> [Term] -> B.ByteString
almuToHtml symbs ts = renderMarkup $ do
    docType
    html $ do
        Html5.head $ meta ! Att.charset "UTF-8"
        body $ mapM_ (almuToHtml' symbs) ts