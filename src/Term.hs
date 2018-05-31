module Term where

import qualified Data.Text as T

-- | Symbols are chosen at runtime and thus cannot
-- be expressed comprehensivly, thus they are stored as text
newtype SymbolName = SymbolName T.Text
    deriving (Eq, Ord)

-- | Environments can be enumerated as they must be implemented
-- by the compiler thus, they are explicitly listed as an ADT
data EnvType = 
    Title
  | Author
  | Date
  | Section
  | Subsection
  | Code
  | Displaymath
  | Math

-- | A term is either an environment, a symbol or text
data Term =
    Env EnvType [Term]
  | Symbol SymbolName
  | Text T.Text
