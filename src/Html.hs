{-# LANGUAGE OverloadedStrings #-}

module Html where

import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Att
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import qualified Data.ByteString.Lazy as B
import Control.Monad (mapM_)

import Term

center :: Html -> Html
center = Html5.div ! Att.style "text-align:center;"

almuToHtml' :: Term -> Html 
almuToHtml' (Text t) = toHtml t
almuToHtml' (Symbol (SymbolName t)) = b $ toHtml t
almuToHtml' (Env Title ts) = center . h1 $ mapM_ almuToHtml' ts
almuToHtml' (Env Author ts) = center . h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Date ts) = center . h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Section ts) = h2 $ mapM_ almuToHtml' ts
almuToHtml' (Env Subsection ts) = h3 $ mapM_ almuToHtml' ts
almuToHtml' (Env Code ts) = pre $ mapM_ almuToHtml' ts
almuToHtml' (Env Displaymath ts) = error "Sorry no math support yet"
almuToHtml' (Env Math ts) = error "Sorry no math support yet"

almuToHtml :: [Term] -> B.ByteString
almuToHtml ts = renderMarkup $ docTypeHtml .  Html5.div . body $ mapM_ almuToHtml' ts