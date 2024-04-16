
module Lambda.PrettyShow(
                         class PrettyShow,
                         prettyShow,
                         parenthesizeIf
                        ) where

import Prelude

-- Like Data.Show but pretty-printing for user consumption, rather
-- than readable Purescript code.
class PrettyShow a where
  prettyShow :: a -> String

-- Helper for pretty-printing functions that need to conditionally
-- parenthesize values, depending on current operator precedence.
parenthesizeIf :: Boolean -> String -> String
parenthesizeIf false v = v
parenthesizeIf true v = "(" <> v <> ")"
