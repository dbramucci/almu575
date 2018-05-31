{-# LANGUAGE OverloadedStrings #-}
module SymbolLookup where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Term (SymbolName (..))

builtIn :: M.Map SymbolName T.Text
builtIn = M.fromList . map (\(a, b) -> (SymbolName a, b)) $ [
    ("lt", "<"),
    ("le", "\x2264"),
    ("gt", ">"),
    ("ge", "\x2265"),
    ("neq", "\x2260"),
    ("forall", "\x2200"),
    ("exists", "\x2203"),
    ("in", "\x2208"),
    ("notin", "\x2209"),
    ("assign", "\x2b60"),
    ("land", "\x2227"),
    ("pagebreak", "<p style=\"page-break-after:always;\"></p>")]